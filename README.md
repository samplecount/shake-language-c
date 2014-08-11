# shake-language-c

**shake-language-c** is a cross-platform build system based on the [Shake][] Haskell library. The focus is on cross-compilation of *C*, *C++* and *Objective C* source code to various target platforms. Currently supported target platforms are *iOS*, *Android NDK*, *Google Portable Native Client*, *MacOS X*, *Linux* and *Windows* (*MinGW*). Supported host platforms are *MacOS X*, *Linux* and *Windows*.

## Examples

Here's an *iOS* example that compiles all `.cpp` files in the `src` directory. The resulting static library `libexample.a` can then be used e.g. from an `XCode` project.

    import Control.Applicative
    import Control.Arrow
    import Development.Shake
    import Development.Shake.FilePath
    import Development.Shake.Language.C
    import qualified Development.Shake.Language.C.Target.OSX as OSX

    main :: IO ()
    main = shakeArgs shakeOptions { shakeFiles = "build/" } $ do
      let target = OSX.target OSX.iPhoneOS (Arm Armv7s)
          toolChain = OSX.toolChain
                        <$> OSX.getSDKRoot
                        <*> (maximum <$> OSX.getPlatformVersions (targetPlatform target))
                        <*> pure target

      lib <- staticLibrary toolChain
              ("build" </> toBuildPrefix target </> "libexample.a")
              (return $ 
                   append compilerFlags [(Just Cpp, ["-std=c++11"])]
               >>> append compilerFlags [(Nothing, ["-O3"])]
               >>> append userIncludes ["include"] )
              (getDirectoryFiles "" ["src//*.cpp"])

      want [lib]

A more complex [build script](https://github.com/samplecount/methcla/tree/develop/Shake_Methcla.hs) is used by the [Methcla](http://methc.la) sound engine library. It defines Shake rules for building the library on various platforms and also exports functions for transparently including the library into other build systems. The build script makes extensive use of Shake [configuration files](https://github.com/samplecount/methcla/tree/develop/config).

## shake build script

Build systems written in Shake normally need to be compiled, see the [Shake manual][shake-manual] for more information. This library contains a simple Haskell script, [shake](https://github.com/samplecount/stir/blob/master/shake), that takes care of all the details given a simple (and optional) configuration file. The only prerequisite for running the script is installing the latest [Haskell platform](http://www.haskell.org/platform/).

[Copy](https://raw.githubusercontent.com/samplecount/stir/master/shake) or link the script to your source folder, write a file `shakefile.hs` with Shake rule definitions and a file `shake.cfg`, at least specifying `shake` as a dependency and also `fclabels` if you're using `shake-language-c` (see below for more configuration options).

    [
        ("dependencies", "fclabels shake")
    ]

You can then initialize the build system by running `./shake .update`. Running the script creates a [Cabal sandbox][cabal-sandbox] and compiles your build system script.

Subsequent runs will and automatically run it with the arguments passed, usually Shake options and target.

### Configuration file

The configuration file is in Haskell syntax containing a simple list of string pairs. The following configuration settings are recognized:

* `dependencies`: Space separated list of package dependencies
* `source-packages`: Space separated list of directories containing Cabal packages (passed to `cabal sandbox add-source`)
* `source-directories`: Space separated list of source module directories (passed to `ghc` during compilation)
* `script`: Path to the build system script (default: `shakefile.hs`)

Here's an example configuration file that depends on three packages, one of them a source package:

    [
        ("dependencies", "fclabels shake filemanip")
      , ("source-packages", "external_libraries/shake")
      , ("source-directories", "external_libraries/stir")
    ]

[shake]: https://github.com/ndmitchell/shake
[shake-manual]: https://github.com/ndmitchell/shake/blob/master/docs/Manual.md
[cabal-sandbox]: http://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes
