module Main where

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
