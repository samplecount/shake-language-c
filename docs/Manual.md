# Manual for shake-language-c

These are the beginnings of a manual for the `shake-language-c` library. Feel free to [submit an issue](https://github.com/samplecount/shake-language-c/issues) for anything you want to see covered.

## Rules

The module `Development.Shake.Language.C.Rules` exports a set of high-level Shake rules for building libraries and executables.

### Locally built libraries

Sometimes you want to structure your project in a set of static libraries that are later linked into one or more executables. For Shake to recognise the libraries as dependencies of the executable you need to add them to the `localLibraries` field of the `BuildFlags` record.

Suppose you declare a static library `libtest.a` and an executable `test` in a Shake rules block, you can set up `libtest.a` as a dependency for `test` like this:

    let toolChain = ...
        buildFlags = ...
    libtest <- staticLibrary toolChain "build/libtest.a"
                (pure buildFlags)
                (pure ["test_lib.c"])
    test <- executable toolChain "build/test"
              -- Modify buildFlags by appending libtest to local libraries
              (pure $ buildFlags . append localLibraries [libtest])
              (pure ["test.c"])
    want [test]
