# Changelog for shake-language-c

## [0.6.0](https://github.com/samplecount/shake-language-c/issues?q=is%3Aopen+is%3Aissue+milestone%3A%22RC+0.6.0%22) - Release candidate

### Added

* Add `Data.Default.Class.Default` instances for some data types; add dependency on package `data-default-class`.

### Changed

* Don't export the entire module `Development.Shake.Language.C.ToolChain` from `Development.Shake.Language.C`; expose `Development.Shake.Language.C.ToolChain` for toolchain writers.
* Export `Development.Shake.Language.C.Language.Language` from `Development.Shake.Language.C.BuildFlags` instead of `Development.Shake.Language.C`.
* Export `Development.Shake.Language.C.Rules` from `Development.Shake.Language.C`; hide `Development.Shake.Language.C.Rules` in Cabal file.
* **Android**: Add `libcxxabi` include directory instead of `gabi++` to include path when compiling with `libcxx`. Fixes `error: no member named '__cxa_demangle' in namespace '__cxxabiv1'`.

### Removed

* Remove `libppapi`, `libppapi_cpp`, `libnacl_io`, `libppapi_simple` from `Development.Shake.Language.C.Target.NaCl`.
* Remove `Development.Shake.Language.C.Target.archString`.

## 0.5.0 - 2014-10-17

First released version.
