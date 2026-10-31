# Changelog for streamly-bytestring

## Unreleased

* Add `Streamly.External.ByteString.Short` for interconversion of
  `ShortByteString` and `Array Word8`.
* `Strict.toArray` copies a `ByteString` that is not in GHC heap memory with
  `memcpy` into a pinned array, instead of copying it one byte at a time into
  an unpinned array.
* `Strict.toArray` returns the shared empty array for every zero length
  `ByteString`.
* Fix `Strict.fromArray` with streamly-core-0.1.0 to copy an unpinned array to
  pinned memory instead of using its address.

## 0.2.4 (Apr 2026)

* Replace gauge with tasty-bench for benchmarks

## 0.2.3 (Sep 2025)

* Support streamly-core-0.3.0

## 0.2.2 (Jul 2024)

* Fix a bug that uses an unpinned array in the foreign pointer

## 0.2.1 (Dec 2023)

* Support streamly-core-0.2.0

## 0.2.0 (Mar 2023)

* Support streamly-core-0.1.0
* Support bytestring >= 0.3.0
* Rename read to reader

## 0.1.4 (Dec 2021)

* Support streamly-0.8.1

## 0.1.3 (Jul 2021)

* Support streamly-0.8.0

## 0.1.2

* Fix memory bug in Strict.toArray

## 0.1.1

* Add `fromChunksIO` function to `Streamly.External.ByteString.Lazy` module.

## 0.1.0.1

* Fix build with ghc-8.0.2 and ghc-7.10.3 ([#6](https://github.com/psibi/streamly-bytestring/issues/5))

## 0.1.0.0

* Initial version released
