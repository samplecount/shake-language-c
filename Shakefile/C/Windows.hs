-- Copyright 2014 Samplecount S.L.
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

module Shakefile.C.Windows (
    toolChain
  , getDefaultToolChain
) where

import           Data.Label (get, set)
import           Development.Shake
import           Shakefile.C
import qualified System.Info as System

getHostArch :: IO Arch
getHostArch = do
    -- TODO: Get the info from the environment
    let arch = System.arch
    return $ case arch of
        "i386" -> X86 I386
        "i686" -> X86 I686
        "x86_64" -> X86 X86_64
        _ -> error $ "Unknown host architecture " ++ arch

target :: Arch -> Target
target = Target Windows (Platform "windows")

toolChain :: ToolChainVariant -> ToolChain
toolChain GCC =
    set variant GCC
  $ set compilerCommand "gcc"
  $ set archiverCommand "ar"
  $ set archiver (\tc flags inputs output -> do
      need inputs
      command_ [] (tool tc archiverCommand)
            $  ["cr"]
            ++ get archiverFlags flags
            ++ [output]
            ++ inputs
      command_ [] (toolFromString tc "ranlib") [output]
    )
  $ set linkerCommand "g++"
  $ defaultToolChain
toolChain LLVM =
    set variant LLVM
  $ set compilerCommand "gcc"
  $ set archiverCommand "ar"
  $ set linkerCommand "g++"
  $ defaultToolChain
toolChain Generic = toolChain GCC

getDefaultToolChain :: IO (Target, Action ToolChain)
getDefaultToolChain = do
    t <- fmap target getHostArch
    return (t, return $ toolChain Generic)
