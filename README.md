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

Build systems written in Shake normally need to be compiled, see the [Shake manual][shake-manual] for more information. This library contains a simple Haskell script, [shake](https://github.com/samplecount/stir/blob/master/shake), that takes care of all the details. By building on the [Cabal][] infrastructure, build scripts can be compiled conveniently and also be reused as library components.

The only prerequisite for running the script is installing the latest [Haskell platform](http://www.haskell.org/platform/).

[Copy](https://raw.githubusercontent.com/samplecount/stir/master/shake) or link the script to your source folder and write a file `shakefile.hs` containing Shake rule definitions. Create a file `shakefile.cabal` with the following contents:

    Name: hearhearme-stirfile
    Version: 0.1.0
    Cabal-Version: >= 1.2
    Build-Type: Simple

    Executable hearhearme-shakefile
      Main-Is: shakefile.hs
      Ghc-Options: -rtsopts -with-rtsopts=-I0
      Build-Depends:
          base == 4.*
        , shake

If you're using `shake-language-c`, add it to the dependencies as well. Refer to the [Cabal manual][cabal] for more information about configuration file features.

Now initialize the build system by running

    ./shake .update

This will create a [Cabal sandbox](http://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes
), install the dependencies and compile your build system script.

Running `./shake` with any argument not starting with a `.` run your build script with the arguments passed on the command line, usually Shake options and targets to build.

    ./shake .scrub

Call's your build script's `clean` target and removes the Cabal sandbox and all build products.

    ./shake .init

Initializes the sandbox and configures your package. This might be necessary sometimes but usually you don't need that command.

### Cabal file extensions

The `shake` script supports the custom Cabal configuration setting `x-stir-package-dirs`. It allows to specify Cabal source packages that should be added to the sandbox with `cabal add-source`, for example:

    X-Stir-Package-Dirs:
      external_libraries/methcla/external_libraries/shake
      external_libraries/methcla/external_libraries/stir
      external_libraries/methcla

Add this setting before the `Executable` section containing your build script. See [here](https://github.com/samplecount/methcla/blob/develop/shakefile.cabal) for an example of a production configuration file that also exports a library.

[cabal]: http://www.haskell.org/cabal/users-guide/
[shake]: https://github.com/ndmitchell/shake
[shake-manual]: https://github.com/ndmitchell/shake/blob/master/docs/Manual.md
