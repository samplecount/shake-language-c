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
    ToolChainVariant(..)
  , toolChain
  , getDefaultToolChain
) where

import Control.Lens
import Data.Version (Version(..))
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

data ToolChainVariant = GCC deriving (Eq, Show)

toolChain :: ToolChainVariant -> ToolChain
toolChain GCC =
    compilerCmd .~ "gcc"
  $ archiverCmd .~ "ar"
  $ linkerCmd .~ "g++"
  $ defaultToolChain

getDefaultToolChain :: IO (Target, ToolChain)
getDefaultToolChain = do
    target <- fmap target getHostArch
    return (target, toolChain GCC)
