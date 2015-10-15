# shake-language-c

[![Hackage version](https://img.shields.io/hackage/v/shake-language-c.svg?style=flat)](https://hackage.haskell.org/package/shake-language-c)
[![Build Status](https://img.shields.io/travis/samplecount/shake-language-c.svg?style=flat&branch=develop)](https://travis-ci.org/samplecount/shake-language-c)

**shake-language-c** is a cross-platform build system based on the [Shake](https://github.com/ndmitchell/shake) Haskell library. The focus is on cross-compilation of *C*, *C++* and *Objective C* source code to various target platforms. Currently supported target platforms are *iOS*, *Android NDK*, *Google Portable Native Client*, *MacOS X*, *Linux* and *Windows* (*MinGW*). Supported host platforms are *MacOS X*, *Linux* and *Windows*.

## Documentation

Please see the [package documentation](http://hackage.haskell.org/package/shake-language-c). Feel free to open an [issue](https://github.com/samplecount/shake-language-c/issues) or send a pull request if there's anything missing that you want to see covered.

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
