-- Copyright 2012-2013 Samplecount S.L.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Shakefile.C.Linux (
    toolChain
  , getDefaultToolChain
) where

import Data.Label (get, set)
import Data.Version (Version(..))
import Development.Shake (need, system')
import Shakefile.C
import System.Process (readProcess)

getHostArch :: IO Arch
getHostArch = do
    arch <- fmap (head.lines) $ readProcess "arch" [] ""
    return $ case arch of
        "i386" -> X86 I386
        "i686" -> X86 I686
        "x86_64" -> X86 X86_64
        _ -> error $ "Unknown host architecture " ++ arch

target :: Arch -> Target
target arch = mkTarget arch "gnu" "linux" (Platform "Linux" (Version [0] []))

archiver_ :: Archiver
archiver_ toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
          $  ["cr"]
          ++ get archiverFlags buildFlags
          ++ [output]
          ++ inputs
    system' (command "ranlib" toolChain) [output]

toolChain :: ToolChainVariant -> ToolChain
toolChain GCC =
    set variant GCC
  $ set compilerCmd "gcc"
  $ set archiverCmd "ar"
  $ set archiver archiver_
  $ set linkerCmd "g++"
  $ defaultToolChain
toolChain LLVM =
    set variant LLVM
  $ set compilerCmd "gcc"
  $ set archiverCmd "ar"
  $ set linkerCmd "g++"
  $ defaultToolChain
toolChain Generic = toolChain GCC

getDefaultToolChain :: IO (Target, ToolChain)
getDefaultToolChain = do
    target <- fmap target getHostArch
    return (target, toolChain GCC)
