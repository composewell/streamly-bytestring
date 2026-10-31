# Audit of streamly-bytestring

Date: 2026-10-31. Audited revision: `b752059` (version 0.2.4), branch
`audit`.

## Scope and method

The audit covers the library modules `Streamly.External.ByteString` and
`Streamly.External.ByteString.Lazy`, the test suite, the cabal file and the
CI configuration. The code was checked for:

* memory safety and correctness bugs,
* interoperability problems with `bytestring` and `streamly-core`,
* missing APIs that users of the package need.

The conversion code was compared with the internals of `bytestring`
0.10.12, 0.11.5 and 0.12.2 (`ForeignPtr` contents, `BS`/`PS` constructors,
`ShortByteString`) and of `streamly-core` 0.1.0, 0.2.0, 0.2.2, 0.3.0 and
0.3.3 (`Array` and `MutByteArray` representation, pinned and unpinned
allocation, `Array.pin`).

The package was built and its test suite run with GHC 9.4.7 (bytestring
0.11.5.2) against every supported streamly-core release; the results are in
the "Verification" section.

## Summary

| ID | Kind | Severity | Status |
|----|------|----------|--------|
| B1 | `Strict.fromArray` uses the address of an unpinned array with streamly-core-0.1.0 | High (memory safety, rare) | Fixed |
| B2 | `Strict.toArray` copies non GHC memory one byte at a time into an unpinned array | Medium (performance) | Fixed |
| B3 | `Strict.toArray` of a zero length slice retains the whole buffer | Low | Fixed |
| I1 | No conversion for `ShortByteString`, the representation of `OsPath` | Medium | Fixed (new module) |
| I2 | Arrays and ByteStrings share memory, mutation through `MutArray` is visible in both | Low | Documented |
| I3 | A ByteString made from an array slice retains the whole underlying buffer | Low | Documented |
| I4 | `Lazy.fromChunksIO` defers IO and its exceptions until the result is evaluated | Low | Documented |
| I5 | API names `write`/`writeN` differ from streamly-core-0.3 names `create`/`createOf` | Low | Recommendation |
| I6 | Cabal file points to the old repository `psibi/streamly-bytestring` | Low | Fixed |
| I7 | `Lazy.reader` and `Lazy.toChunks` use the deprecated `readChunks` | Low | Fixed |
| M1 | No fold from a stream of `Word8` or `Array Word8` to a lazy ByteString | Low | Recommendation |
| T1 | Tests did not cover non GHC memory, slices, unpinned arrays, chunk structure | Medium | Fixed |
| C1 | CI did not pin streamly-core 0.3.1, 0.3.2 and 0.3.3 | Low | Fixed |
| H1 | Build and test commands have no heap limit | Low | Proposal |

## Bugs

### B1: `fromArray` with an unpinned array on streamly-core-0.1.0

`Strict.fromArray` builds a `ForeignPtr` from the address of the array's
`MutableByteArray#`. The address is valid only if the byte array is pinned;
the GC moves unpinned byte arrays during a collection.

For streamly-core 0.2.0 and later, `fromArray` calls `Array.pin`, which
copies an unpinned array to pinned memory. For streamly-core-0.1.0,
`ensurePinned` was `pure`, on the assumption that every array is pinned.
streamly-core-0.1.0 allocates arrays with `newPinned`, except arrays created
by `Streamly.Internal.Data.Array.Mut.Type.new`, which uses
`newUnpinnedBytes`. A ByteString made from such an array points to memory
that the GC can move or reuse.

Fix: for streamly-core-0.1.0, `ensurePinned` checks
`isMutableByteArrayPinned#` and copies an unpinned array slice to a new
pinned byte array.

### B2: byte-by-byte copy of non GHC memory in `toArray`

`Strict.toArray` is `O(1)` when the ByteString's `ForeignPtr` contents is
`PlainPtr`, the case for memory allocated by bytestring itself. For every
other `ForeignPtr` it copies the bytes. ByteStrings of this kind come from
string literals (`unsafePackLenLiteral`, `FinalPtr`), `mmap` based
libraries, `unsafePackCStringFinalizer` and `mallocForeignPtrBytes`
(`MallocPtr`).

The copy was a `Fold` over an `Unfold` that `peek`s one byte at a time, into
an array created by `Array.createOf` (or `Array.writeN`). On streamly-core
0.2.0 and later that array is unpinned, so `fromArray (toArray bs)` copied a
second time to pin it.

Fix: allocate a pinned byte array with `newPinnedByteArray#` and copy with
`copyAddrToByteArray#` (a `memcpy`). `fromArray . toArray` now copies once
for such ByteStrings. The code no longer needs the internal `Unfold.fold`,
`mkUnfoldrM` and `Step` imports, which differ between streamly-core
versions.

### B3: zero length slice retains the buffer

`toArray` returned the shared empty array only for a null pointer. A zero
length slice with a non-null pointer, for example `unsafeTake 0 bs`, gave an
array that referenced the whole buffer of `bs`. `toArray` now returns the
shared empty array for every zero length ByteString.

### Checked and correct

* The offset computation in `toArray` for `PlainPtr`:
  `addr - byteArrayContents marr` for bytestring 0.11 and later, plus the
  `PS` offset for bytestring 0.10.12.
* `toArray` does not take the `MallocPtr` contents apart. A `MallocPtr`
  holds finalizers on an `IORef`; sharing only the `MutableByteArray#` would
  let the finalizers run while the array is still in use. Copying is the
  correct choice.
* `fromArray` returns `mempty` for an empty array, so it never builds a
  `ForeignPtr` to an empty unpinned array.
* `Array.pin` in streamly-core 0.2.0 and later treats large byte arrays
  (implicitly pinned) as pinned, through `isMutableByteArrayPinned#`.
* `Lazy.fromChunks` and `Lazy.fromChunksIO` use `chunk`, which drops empty
  chunks, so the result satisfies the lazy ByteString invariant.

## Interoperability

### I1: `ShortByteString`

`ShortByteString` is a `ByteArray#`, pinned or unpinned. It is the
representation of `PosixString`, `WindowsString`, `OsString` and `OsPath` in
the `os-string` and `filepath` packages. The package had no conversion
between `ShortByteString` and `Array Word8`.

Added `Streamly.External.ByteString.Short` with `toArray`, `fromArray`,
`reader`, `writeN` and `write`:

* `toArray` is `O(1)`; the array shares the `ByteArray#`.
* `fromArray` is `O(1)` when the array starts at offset 0 and ends at the
  size of its byte array (`getSizeofMutableByteArray#`). Otherwise it copies
  the slice into a new unpinned byte array.

The conversions use only GHC primops and the `Array` constructor, which has
the same fields in every supported streamly-core version. `writeN` and `write`
use `Array.createOf`/`Array.create` on streamly-core-0.3 and
`Array.writeN`/`Array.write` on older versions.

### I2: shared memory

`Strict.toArray` of a GHC allocated ByteString, `Strict.fromArray` of a
pinned array, `Short.toArray`, and `Short.fromArray` of a whole array share
memory between the two values. Both types are immutable, so this is safe as
long as no code mutates the memory. Code that obtains a `MutArray` with
`unsafeThaw` and writes to it changes the ByteString as well. No change was
made; this is the documented contract of `unsafeThaw`.

### I3: retained buffers

`Strict.fromArray` of an array slice keeps the whole underlying byte array
alive. Examples:

* a 100 byte ByteString made from a slice of a 1 MiB pinned array keeps the
  1 MiB array alive,
* `Strict.writeN n` with a stream shorter than `n` allocates `n` bytes and
  the ByteString keeps all `n`.

Users who keep small ByteStrings for a long time can apply `BS.copy`. No
change was made.

### I4: lazy IO in `fromChunksIO`

`Lazy.fromChunksIO` uses `unsafeInterleaveIO`. The stream is run as the
lazy ByteString is evaluated, so an exception raised by the stream is raised
at the point of evaluation, and a resource used by the stream (for example a
`Handle` closed by `bracket`) must stay open until the ByteString is fully
evaluated. This is the standard lazy IO behaviour, the same as
`Data.ByteString.Lazy.hGetContents`. No change was made.

### I5: API names

streamly-core-0.3 renamed `Array.write` to `Array.create` and `Array.writeN`
to `Array.createOf`, and deprecated the old names. This package still uses
`write` and `writeN` in `Streamly.External.ByteString` and, for consistency,
in the new `Streamly.External.ByteString.Short`.

Recommendation: add `create` and `createOf` to both modules and deprecate
`write` and `writeN` in a later release.

### I6: repository URLs

`homepage`, `bug-reports`, `description` and `source-repository` in the
cabal file pointed to `github.com/psibi/streamly-bytestring`. They now point
to `github.com/composewell/streamly-bytestring`. `tested-with` now lists the
GHC versions used in CI (8.10.7 to 9.14.1).

### I7: deprecated names used internally

`Lazy.reader` and `Lazy.toChunks` used `readChunks`, the deprecated alias of
`chunkReader`. They now use `chunkReader`. The same commit fixes the
`chunkReader` haddock (`Array Word8`, not `Array Words`), removes a
commented out copy of the `LazyIO` example, and wraps lines to 79 columns.

## Missing APIs

* `Streamly.External.ByteString.Short` (I1): added.
* M1: there is no `Fold` that produces a lazy ByteString, for example
  `Fold m (Array Word8) Lazy.ByteString` or a fold from `Word8` that creates
  chunks of a given size. `Lazy.fromChunks` takes a whole `Stream`, so a
  lazy ByteString cannot be produced inside a composed fold. Recommendation:
  add `Lazy.chunkWriter :: Monad m => Fold m (Array Word8) ByteString`
  (strict accumulation of chunks) if users ask for it.
* `create`/`createOf` names (I5): recommendation.

No conversion is missing between strict ByteString and `Array Word8`, or
between lazy ByteString and a stream of `Array Word8`.

## Tests

The test suite had 21 examples. It now has 50. New cases:

Strict:

* `toArray`/`fromArray` of a `PlainPtr` ByteString share memory (same
  address).
* ByteStrings outside the GHC heap: a string literal
  (`unsafePackAddressLen`), `mallocForeignPtrBytes`, and
  `unsafePackCStringFinalizer`. The test checks contents, round trip, and
  that the copy made by `toArray` is pinned (a second round trip keeps the
  address).
* `BS.empty`, a zero length slice, and an empty array.
* Arbitrary slices of ByteStrings and of arrays, pinned and unpinned.
* `fromArray` of an unpinned array, followed by major GCs and allocation;
  the ByteString must keep its contents.
* `write` produces pinned memory; `writeN n` for `n` less than, equal to and
  more than the input length.

Lazy:

* `chunkReader` and `toChunks` produce the same chunks as `BSL.toChunks`.
* `fromChunks` and `fromChunksIO` do not produce empty chunks.
* `fromChunks . toChunks` keeps the chunk structure.
* `fromChunks` is lazy in `Identity`; `reader` works on an infinite lazy
  ByteString.

Short: round trip, zero copy for a whole array, copy of a slice, conversion
of a pinned ByteString array, `reader`, `write` and `writeN`.

The unpinned array tests detect bug B1: with `ensurePinned = pure` on
streamly-core-0.3.3, 2 of the 50 examples fail.

## CI

The CI had jobs for streamly-core 0.1.0, 0.2.0, 0.2.1, 0.2.2, 0.2.3, 0.3.0,
the latest Hackage release and git master. Added
`cabal.project.d/streamly-core-0.3.{1,2,3}` and a job for each, on GHC
9.12.4, 9.8.4 and 9.10.3. Every release allowed by the bound
`streamly-core >= 0.1.0 && < 0.4.1` now has a pinned job.

## Proposal: heap limits (H1)

The CI and the documented build commands do not limit the GHC heap or the
test heap. A runaway compilation or test can use all the memory of the
machine. Proposal, not yet applied:

* add `ghc-options: +RTS -M3G -RTS` for the package in each
  `cabal.project.d` file, to limit the compiler heap;
* run the test suite with `+RTS -M1G -RTS` (the test is linked with
  `-rtsopts`), for example through packcheck `TEST_OPTIONS` or
  `cabal test --test-options`.

## Verification

All builds used GHC 9.4.7 and bytestring 0.11.5.2, with `+RTS -M3G -RTS` for
GHC and `+RTS -M2G -RTS` for the test suite.

| streamly-core | Library and test build | Warnings | Test suite |
|---------------|------------------------|----------|------------|
| 0.1.0 | OK | 0 | 50 examples, 0 failures |
| 0.2.0 | OK | 0 | 50 examples, 0 failures |
| 0.2.1 | OK | 0 | 50 examples, 0 failures |
| 0.2.2 | OK | 0 | 50 examples, 0 failures |
| 0.2.3 | OK | 0 | 50 examples, 0 failures |
| 0.3.0 | OK | 0 | 50 examples, 0 failures |
| 0.3.1 | OK | 0 | 50 examples, 0 failures |
| 0.3.2 | OK | 0 | 50 examples, 0 failures |
| 0.3.3 | OK | 0 | 50 examples, 0 failures |

Before the changes, the original test suite passed on streamly-core-0.3.3
with 21 examples, 0 failures.

Not verified locally: bytestring 0.10.12 (needs an older GHC), GHC versions
other than 9.4.7, macOS, Windows, and hlint. The CI covers these.
