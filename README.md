# streamly-bytestring

Library for streamly and bytestring interoperation.

If you are writing code from scratch, please use `Streamly.Data.Array` which is
a generalization of `ByteString` and better integrated with streamly.

This library is to enable interoperation of streamly with existing code that
uses `ByteString`.

The package provides APIs to interconvert between:

* strict `ByteString` and streamly `Array Word8`
  (`Streamly.External.ByteString`)
* lazy `ByteString` and a stream of `Array Word8`
  (`Streamly.External.ByteString.Lazy`)
* `ShortByteString` and streamly `Array Word8`
  (`Streamly.External.ByteString.Short`)

The interconversion of strict `ByteString` and `Array Word8` has no overhead
for GHC allocated memory. A `ByteString` in memory allocated outside the GHC
heap is copied to a pinned array. An unpinned array is copied to pinned memory
when converted to a `ByteString`.

`ShortByteString` to `Array Word8` has no overhead. `Array Word8` to
`ShortByteString` has no overhead when the array covers its whole underlying
byte array, otherwise the array is copied.

## Usage

This is a dumb program that counts the number of bytes in a file.

```haskell
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as FL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy

import System.IO (FilePath)

strictByteStringSize :: BS.ByteString -> IO Int
strictByteStringSize bs = S.fold FL.length $ S.unfold Strict.reader bs

lazyByteStringSize :: BSL.ByteString -> IO Int
lazyByteStringSize bsl =
    S.fold (FL.foldl' (+) 0)
        $ S.mapM strictByteStringSize
        $ fmap Strict.fromArray
        $ Lazy.toChunks bsl

fileSize :: FilePath -> IO Int
fileSize path = do
    bsl <- BSL.readFile path
    lazyByteStringSize bsl
```
