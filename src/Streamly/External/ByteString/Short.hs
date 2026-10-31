{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Interconversion between 'ShortByteString' and streamly 'Array' 'Word8'.
--
-- A 'ShortByteString' is a GHC @ByteArray#@, pinned or unpinned. An 'Array'
-- is a slice of a GHC @MutableByteArray#@, pinned or unpinned. 'toArray' is
-- always @O(1)@. 'fromArray' is @O(1)@ when the array slice covers its whole
-- underlying byte array, otherwise the slice is copied.
--
-- 'ShortByteString' is the representation used by @OsString@ and @OsPath@ in
-- the @os-string@ and @filepath@ packages.
module Streamly.External.ByteString.Short
  ( toArray
  , fromArray

  , reader

  , writeN
  , write
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import GHC.Exts
    ( copyMutableByteArray#
    , getSizeofMutableByteArray#
    , isTrue#
    , newByteArray#
    , sizeofByteArray#
    , unsafeCoerce#
    , unsafeFreezeByteArray#
    , (-#)
    , (==#)
    )
import GHC.Int (Int(..))
import GHC.IO (IO(..))
import Streamly.Data.Fold (Fold)
import Streamly.Data.Unfold (Unfold, lmap)

-- Internal imports
import Data.ByteString.Short.Internal (ShortByteString(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Data.Array as Array

#if MIN_VERSION_streamly_core(0,2,0)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
#else
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Unboxed (MutableByteArray(..))
#endif

#if MIN_VERSION_streamly_core(0,2,0)
#define MUT_BYTE_ARRAY MutByteArray
#else
#define MUT_BYTE_ARRAY MutableByteArray
#endif

#if MIN_VERSION_streamly_core(0,3,0)
#define CREATE_N Array.createOf
#define CREATE Array.create
#else
#define CREATE_N Array.writeN
#define CREATE Array.write
#endif

-- | Convert a 'ShortByteString' to an array of 'Word8' in constant time. The
-- array shares the memory of the 'ShortByteString'.
{-# INLINE toArray #-}
toArray :: ShortByteString -> Array Word8
toArray (SBS ba#) =
    Array (MUT_BYTE_ARRAY (unsafeCoerce# ba#)) 0 (I# (sizeofByteArray# ba#))

-- | Convert an array of 'Word8' to a 'ShortByteString'.
--
-- If the array starts at offset 0 and ends at the end of its underlying byte
-- array, the 'ShortByteString' shares the memory of the array, in constant
-- time. Otherwise the bytes of the array are copied to a new unpinned byte
-- array.
{-# INLINE fromArray #-}
fromArray :: Array Word8 -> ShortByteString
fromArray (Array (MUT_BYTE_ARRAY marr#) (I# start#) (I# end#)) =
    unsafeInlineIO $ IO $ \s0 ->
        case getSizeofMutableByteArray# marr# s0 of
            (# s1, size# #)
                | isTrue# (start# ==# 0#) && isTrue# (end# ==# size#) ->
                    case unsafeFreezeByteArray# marr# s1 of
                        (# s2, ba# #) -> (# s2, SBS ba# #)
                | otherwise ->
                    let len# = end# -# start#
                     in case newByteArray# len# s1 of
                        (# s2, marr1# #) ->
                            case copyMutableByteArray#
                                    marr# start# marr1# 0# len# s2 of
                                s3 ->
                                    case unsafeFreezeByteArray# marr1# s3 of
                                        (# s4, ba# #) -> (# s4, SBS ba# #)

-- | Unfold a 'ShortByteString' to a stream of 'Word8'.
{-# INLINE reader #-}
reader :: Monad m => Unfold m ShortByteString Word8
reader = lmap toArray Array.reader

-- | Fold a stream of 'Word8' to a 'ShortByteString' of the given size in
-- bytes.
{-# INLINE writeN #-}
writeN :: MonadIO m => Int -> Fold m Word8 ShortByteString
writeN i = fromArray <$> CREATE_N i

-- | Fold a stream of 'Word8' to a 'ShortByteString' of appropriate size.
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ShortByteString
write = fromArray <$> CREATE
