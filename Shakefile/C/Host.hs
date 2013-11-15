module Shakefile.C.Host (
    getDefaultToolChain
) where

import           Shakefile.C (Target, ToolChain)
import qualified Shakefile.C.OSX as OSX
import           System.Info (os)

getDefaultToolChain :: IO (Target, ToolChain)
getDefaultToolChain
    | os == "darwin" = OSX.getDefaultToolChain
    | os == "mingw" = error "No default toolchain for Windows yet"
    | os == "linux" = error "No default toolchain for Linux yet"
    | otherwise = error $ "No default toolchain for this operating system (" ++ os ++ ")"
