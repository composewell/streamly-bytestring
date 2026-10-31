{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Control.Monad (replicateM_, when)
import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Exts
    ( copyMutableByteArray#
    , isMutableByteArrayPinned#
    , isTrue#
    , newByteArray#
    , MutableByteArray#
    , RealWorld
    , sameMutableByteArray#
    , unsafeCoerce#
    , (-#)
    )
import GHC.Int (Int(..))
import GHC.IO (IO(..))
import GHC.IO.Handle (Handle)
import Streamly.FileSystem.Handle (chunkReader)
import System.IO (openFile, IOMode(ReadMode))
import System.IO.Temp (withSystemTempFile)
import System.Mem (performMajorGC)
import System.Random (randomIO)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, (===), (==>), choose, forAll)
import Test.QuickCheck.Instances.ByteString ()

-- Internal imports
import Data.ByteString.Internal (fromForeignPtr, toForeignPtr)
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.ByteString.Unsafe
    (unsafePackAddressLen, unsafePackCStringFinalizer)
#if MIN_VERSION_streamly_core(0,2,0)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
#else
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Unboxed (MutableByteArray(..))
#endif

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy
import qualified Streamly.External.ByteString.Short as Short

#if MIN_VERSION_streamly_core(0,2,0)
#define MUT_BYTE_ARRAY MutByteArray
#else
#define MUT_BYTE_ARRAY MutableByteArray
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

arrLength :: Array Word8 -> Int
arrLength arr = arrEnd arr - arrStart arr

-- | Address of the first byte of a ByteString.
bsPtr :: ByteString -> Ptr Word8
bsPtr bs =
    let (fp, off, _) = toForeignPtr bs
     in unsafeForeignPtrToPtr fp `plusPtr` off

-- | A ByteString in memory allocated by mallocForeignPtrBytes (MallocPtr).
mallocByteString :: [Word8] -> IO ByteString
mallocByteString xs = do
    let n = length xs
    fp <- mallocForeignPtrBytes n
    withForeignPtr fp $ \p -> pokeArray p xs
    pure $ fromForeignPtr fp 0 n

-- | A ByteString in memory allocated by malloc and freed by a finalizer.
finalizerByteString :: [Word8] -> IO ByteString
finalizerByteString xs = do
    let n = length xs
    p <- mallocBytes n
    pokeArray p xs
    unsafePackCStringFinalizer p n (free p)

-- | Copy an array to a new unpinned byte array. Small byte arrays allocated
-- by newByteArray# are unpinned and can be moved by the garbage collector.
unpinnedCopy :: Array Word8 -> IO (Array Word8)
unpinnedCopy (Array (MUT_BYTE_ARRAY src#) (I# start#) (I# end#)) =
    IO $ \s0 ->
        let len# = end# -# start#
         in case newByteArray# len# s0 of
            (# s1, dst# #) ->
                case copyMutableByteArray# src# start# dst# 0# len# s1 of
                    s2 -> (# s2, Array (MUT_BYTE_ARRAY dst#) 0 (I# len#) #)

isPinnedArray :: Array Word8 -> Bool
isPinnedArray (Array (MUT_BYTE_ARRAY marr#) _ _) =
    isTrue# (isMutableByteArrayPinned# marr#)

-- | Run major GCs and allocate garbage so that unpinned objects move and
-- their old memory is reused.
churnHeap :: IO ()
churnHeap =
    replicateM_ 3 $ do
        performMajorGC
        threadDelay 10000
        _ <- evaluate
                $ Array.length
                $ Array.fromListN 10000 (cycle [0 .. 255 :: Word8])
        performMajorGC

word8List :: Int -> IO [Word8]
word8List l =
    S.fold Fold.toList $
    S.take l $
    fmap fromIntegral $
    fmap (\x -> abs x `mod` 256) $ S.repeatM (randomIO :: IO Int)

-------------------------------------------------------------------------------
-- Strict: toArray/fromArray
-------------------------------------------------------------------------------

propStrictRoundTrip :: ByteString -> Property
propStrictRoundTrip bs = Strict.fromArray (Strict.toArray bs) === bs

propStrictRoundTripSlice :: ByteString -> Property
propStrictRoundTripSlice bs =
    forAll (choose (0, BS.length bs)) $ \i ->
    forAll (choose (0, BS.length bs - i)) $ \n ->
        let bs1 = BS.take n (BS.drop i bs)
         in Strict.fromArray (Strict.toArray bs1) === bs1

propStrictToArray :: ByteString -> Property
propStrictToArray bs =
    let arr = Strict.toArray (BS.drop 5 bs)
     in Array.toList arr === BS.unpack (BS.drop 5 bs)

-- toArray shares the memory of a GHC allocated (PlainPtr) ByteString.
propStrictZeroCopy :: ByteString -> Property
propStrictZeroCopy bs =
    not (BS.null bs) ==>
        let bs1 = BS.copy bs
         in bsPtr (Strict.fromArray (Strict.toArray bs1)) === bsPtr bs1

testStrictForeign :: String -> ([Word8] -> IO ByteString) -> Spec
testStrictForeign name mk =
    describe name $ do
        prop "toArray contents" $ \xs -> do
            bs <- mk xs
            Array.toList (Strict.toArray bs) `shouldBe` xs
        prop "fromArray . toArray = id" $ \xs -> do
            bs <- mk xs
            Strict.fromArray (Strict.toArray bs) `shouldBe` bs
        prop "toArray copies to pinned memory" $ \xs -> do
            bs <- mk xs
            let bs1 = Strict.fromArray (Strict.toArray bs)
            when (not (null xs)) $
                bsPtr (Strict.fromArray (Strict.toArray bs1))
                    `shouldBe` bsPtr bs1

-- A ByteString pointing to a string literal is not GHC heap memory.
testStrictLiteral :: Spec
testStrictLiteral =
    it "literal ByteString" $ do
        lit <- unsafePackAddressLen 5 "hello"#
        let xs = [104, 101, 108, 108, 111]
        Array.toList (Strict.toArray lit) `shouldBe` xs
        Strict.fromArray (Strict.toArray lit) `shouldBe` BS.pack xs

testFromArrayUnpinned :: Spec
testFromArrayUnpinned =
    prop "fromArray of an unpinned array" $
        forAll (choose (1, 1000)) $ \n -> do
            xs <- word8List n
            arr <- unpinnedCopy (Strict.toArray (BS.pack xs))
            isPinnedArray arr `shouldBe` False
            let bs = Strict.fromArray arr
            BS.length bs `seq` churnHeap
            BS.unpack bs `shouldBe` xs
            Array.toList arr `shouldBe` xs

testFromArraySlice :: Spec
testFromArraySlice =
    prop "fromArray of an array slice" $ \xs ->
        forAll (choose (0, length xs)) $ \i ->
        forAll (choose (0, length xs - i)) $ \n -> do
            let arr = Strict.toArray (BS.pack xs)
                slice = arr { arrStart = arrStart arr + i
                            , arrEnd = arrStart arr + i + n }
            BS.unpack (Strict.fromArray slice)
                `shouldBe` take n (drop i xs)
            uslice <- unpinnedCopy slice
            BS.unpack (Strict.fromArray uslice)
                `shouldBe` take n (drop i xs)

testStrictEmpty :: Spec
testStrictEmpty =
    describe "empty" $ do
        it "toArray BS.empty" $
            arrLength (Strict.toArray BS.empty) `shouldBe` 0
        it "toArray of a zero length slice" $
            arrLength (Strict.toArray (BS.take 0 (BS.pack [1, 2, 3])))
                `shouldBe` 0
        it "fromArray of an empty array" $
            Strict.fromArray (Array.fromList []) `shouldBe` BS.empty

-- Regression test: fromArray must not return a ByteString that points to an
-- unpinned array that the GC can move.
checkPinnedNature :: IO ()
checkPinnedNature = do
    (arr :: Array Word8) <-
        evaluate (Array.fromList (take 1000 (cycle [0..255])))
    churnHeap
    (_ :: Array Word8) <-
        evaluate (Array.fromList (take 10000 (cycle [0..255])))
    let bs = Strict.fromArray arr
    BS.length bs `seq` churnHeap
    let lst1 = BS.unpack bs
        lst2 = Array.toList arr
    when (lst1 /= lst2) $ error "Pinned nature isn't ensured"

streamToByteString :: Monad m => S.Stream m (Array Word8) -> m ByteString
streamToByteString stream =
    S.fold (Fold.foldl' (<>) mempty) $ fmap Strict.fromArray stream

checkFileContent :: FilePath -> Handle -> IO ()
checkFileContent filename handle' = do
    bsContent <- BS.hGetContents handle'
    handle <- openFile filename ReadMode
    bsStreamly <- streamToByteString $ S.unfold chunkReader handle
    bsContent `shouldBe` bsStreamly

-------------------------------------------------------------------------------
-- Strict: reader, write, writeN
-------------------------------------------------------------------------------

propFoldTestStrict :: [Word8] -> Spec
propFoldTestStrict bl =
    prop
        ("Strict: fold write . fromList = pack" ++
         " -- Size: " ++ show (length bl) ++ " bytes") $ do
        bs <- S.fold Strict.write $ S.fromList bl
        BS.unpack bs `shouldBe` bl
        -- write produces pinned memory, fromArray . toArray does not copy
        when (not (null bl)) $
            bsPtr (Strict.fromArray (Strict.toArray bs)) `shouldBe` bsPtr bs

propNFoldTestStrict :: Int -> [Word8] -> Spec
propNFoldTestStrict n bl =
    prop
        ("Strict: fold (writeN n) . fromList = pack . take n" ++
         " -- Size: " ++ show n ++ " bytes") $ do
        bl' <- fmap BS.unpack $ S.fold (Strict.writeN n) $ S.fromList bl
        bl' `shouldBe` take n bl

propWriteNVarying :: Spec
propWriteNVarying =
    prop "Strict: writeN n with n less than, equal to or more than input" $
        \(xs :: [Word8]) -> forAll (choose (0, length xs + 10)) $ \n -> do
            bs <- S.fold (Strict.writeN n) (S.fromList xs)
            BS.unpack bs `shouldBe` take n xs

propUnfoldTestStrict :: [Word8] -> Spec
propUnfoldTestStrict bl =
    prop
        ("Strict: toList . unfold read . pack = id" ++
         " -- Size: " ++ show (length bl) ++ " bytes") $ do
        bl' <- S.fold Fold.toList (S.unfold Strict.reader (BS.pack bl))
        bl' `shouldBe` bl

propReaderSlice :: Spec
propReaderSlice =
    prop "Strict: reader of a sliced ByteString" $ \bs -> do
        let bs1 = BS.drop 3 (BS.take (BS.length bs - 2) bs)
        xs <- S.fold Fold.toList (S.unfold Strict.reader bs1)
        xs `shouldBe` BS.unpack bs1

-------------------------------------------------------------------------------
-- Lazy
-------------------------------------------------------------------------------

propUnfoldTestLazy :: [Word8] -> Spec
propUnfoldTestLazy bl =
    prop
        ("Lazy: toList . unfold read . pack = id" ++
         " -- Size: " ++ show (length bl) ++ " bytes") $ do
        bl' <- S.fold Fold.toList (S.unfold Lazy.reader (BSL.pack bl))
        bl' `shouldBe` bl

propLazyReader :: Spec
propLazyReader =
    prop "Lazy: reader of a multi chunk ByteString" $ \lbs -> do
        xs <- S.fold Fold.toList (S.unfold Lazy.reader lbs)
        xs `shouldBe` BSL.unpack lbs

propChunkReader :: Spec
propChunkReader =
    prop "Lazy: chunkReader preserves chunks" $ \lbs -> do
        arrs <- S.fold Fold.toList (S.unfold Lazy.chunkReader lbs)
        map Strict.fromArray arrs `shouldBe` BSL.toChunks lbs

propToChunks :: Spec
propToChunks =
    prop "Lazy: toChunks preserves chunks" $ \lbs -> do
        arrs <- S.fold Fold.toList (Lazy.toChunks lbs)
        map Strict.fromArray arrs `shouldBe` BSL.toChunks lbs

propFromChunks :: Spec
propFromChunks =
    prop "Lazy.fromChunks = BSL.fromChunks" $ \aL -> do
        x1 <- Lazy.fromChunks $ fmap Strict.toArray $ S.fromList aL
        let x2 = BSL.fromChunks aL
        x1 `shouldBe` x2
        -- A lazy ByteString must not contain empty chunks
        any BS.null (BSL.toChunks x1) `shouldBe` False

propFromChunksIO :: Spec
propFromChunksIO =
    prop "Lazy.fromChunksIO = BSL.fromChunks" $ \aL -> do
        x1 <- Lazy.fromChunksIO $ fmap Strict.toArray $ S.fromList aL
        let x2 = BSL.fromChunks aL
        x1 `shouldBe` x2
        any BS.null (BSL.toChunks x1) `shouldBe` False

propLazyRoundTrip :: Spec
propLazyRoundTrip =
    prop "Lazy: fromChunks . toChunks = id" $ \lbs -> do
        lbs2 <- Lazy.fromChunks (Lazy.toChunks lbs)
        lbs2 `shouldBe` lbs
        BSL.toChunks lbs2 `shouldBe` BSL.toChunks lbs

testFromChunksIOLaziness :: Word8 -> IO Word8
testFromChunksIOLaziness h = do
    lbs <-
        Lazy.fromChunksIO $
        S.fromList (Strict.toArray (BS.singleton h) : undefined)
    return $ BSL.head lbs

testFromChunksLaziness :: Word8 -> Word8
testFromChunksLaziness h =
    BSL.head
        $ runIdentity
        $ Lazy.fromChunks
        $ S.fromList (Strict.toArray (BS.singleton h) : undefined)

testLazyReaderInfinite :: IO [Word8]
testLazyReaderInfinite =
    S.fold Fold.toList
        $ S.take 10
        $ S.unfold Lazy.reader (BSL.cycle (BSL.pack [1, 2, 3]))

-------------------------------------------------------------------------------
-- Short
-------------------------------------------------------------------------------

propShortRoundTrip :: Spec
propShortRoundTrip =
    prop "Short: fromArray . toArray = id" $ \xs -> do
        let sbs = SBS.pack xs
        Short.fromArray (Short.toArray sbs) `shouldBe` sbs
        Array.toList (Short.toArray sbs) `shouldBe` xs

-- toArray and fromArray of a whole array share the byte array.
propShortZeroCopy :: Spec
propShortZeroCopy =
    prop "Short: toArray and fromArray of a whole array do not copy" $ \xs ->
        let sbs = SBS.pack xs
         in case (sbs, Short.fromArray (Short.toArray sbs)) of
            (SBS ba#, SBS ba1#) ->
                isTrue#
                    (sameMutableByteArray#
                        (unsafeCoerce# ba# :: MutableByteArray# RealWorld)
                        (unsafeCoerce# ba1#))

propShortFromArraySlice :: Spec
propShortFromArraySlice =
    prop "Short: fromArray of an array slice" $ \xs ->
        forAll (choose (0, length xs)) $ \i ->
        forAll (choose (0, length xs - i)) $ \n -> do
            let arr = Short.toArray (SBS.pack xs)
                slice = arr { arrStart = i, arrEnd = i + n }
                sbs = Short.fromArray slice
            SBS.unpack sbs `shouldBe` take n (drop i xs)
            SBS.length sbs `shouldBe` n

propShortFromArrayStrict :: Spec
propShortFromArrayStrict =
    prop "Short: fromArray of a pinned ByteString array" $ \bs ->
        Short.fromArray (Strict.toArray bs) `shouldBe` SBS.toShort bs

propShortReader :: Spec
propShortReader =
    prop "Short: toList . unfold reader = unpack" $ \xs -> do
        ys <- S.fold Fold.toList (S.unfold Short.reader (SBS.pack xs))
        ys `shouldBe` xs

propShortWrite :: Spec
propShortWrite =
    prop "Short: fold write . fromList = pack" $ \(xs :: [Word8]) -> do
            sbs <- S.fold Short.write (S.fromList xs)
            sbs `shouldBe` SBS.pack xs

propShortWriteN :: Spec
propShortWriteN =
    prop "Short: fold (writeN n) . fromList = pack . take n" $
        \(xs :: [Word8]) -> forAll (choose (0, length xs + 10)) $ \n -> do
            sbs <- S.fold (Short.writeN n) (S.fromList xs)
            sbs `shouldBe` SBS.pack (take n xs)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    hspec $ do
        describe "Strict: toArray/fromArray" $ do
            it "Pinned in nature" checkPinnedNature
            it "Strict fromArray" $
                mapM_
                    (flip withSystemTempFile checkFileContent . show)
                    ([1 .. 100] :: [Int])
            prop "Strict Identity" propStrictRoundTrip
            prop "Strict Identity (with offset)" $ \bs ->
                propStrictRoundTrip (BS.drop 5 bs)
            prop "Strict Identity (slices)" propStrictRoundTripSlice
            prop "toArray contents" propStrictToArray
            prop "toArray never produces negative length" $ \bs ->
                -- 'BS.drop 5' to trigger non-zero offset
                arrLength (Strict.toArray (BS.drop 5 bs)) >= 0
            prop "toArray/fromArray share PlainPtr memory" propStrictZeroCopy
            testStrictEmpty
            testStrictLiteral
            testStrictForeign "MallocPtr ByteString" mallocByteString
            testStrictForeign "finalizer ByteString" finalizerByteString
            testFromArrayUnpinned
            testFromArraySlice
        wl0 <- runIO $ word8List 0
        wlM <- runIO $ word8List 900
        wlL <- runIO $ word8List 73700
        describe "Strict: Fold tests" $ do
            propFoldTestStrict wl0
            propFoldTestStrict wlM
            propFoldTestStrict wlL
        describe "Strict: N Fold tests" $ do
            propNFoldTestStrict 0 wl0
            propNFoldTestStrict 900 wlM
            propNFoldTestStrict 73700 wlL
            propWriteNVarying
        describe "Strict: Unfold tests" $ do
            propUnfoldTestStrict wl0
            propUnfoldTestStrict wlM
            propUnfoldTestStrict wlL
            propReaderSlice
        describe "Lazy: Unfold tests" $ do
            propUnfoldTestLazy wl0
            propUnfoldTestLazy wlM
            propUnfoldTestLazy wlL
            propLazyReader
            propChunkReader
            it "reader of an infinite ByteString" $ do
                xs <- testLazyReaderInfinite
                xs `shouldBe` [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
        describe "Lazy: chunks" $ do
            propToChunks
            propLazyRoundTrip
        describe "Laziness of fromChunksIO" $ do
            it "Should not fail" $ do
                w <- testFromChunksIOLaziness 100
                w `shouldBe` 100
        describe "Laziness of fromChunks in Identity" $
            it "Should not fail" $ testFromChunksLaziness 100 `shouldBe` 100
        describe "Correctness of fromChunks" propFromChunks
        describe "Correctness of fromChunksIO" propFromChunksIO
        describe "Short" $ do
            propShortRoundTrip
            propShortZeroCopy
            propShortFromArraySlice
            propShortFromArrayStrict
            propShortReader
            propShortWrite
            propShortWriteN
