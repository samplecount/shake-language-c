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

{-|
Description: Toolchain definitions and utilities for Windows

This module provides toolchain definitions and utilities for targeting Windows.
See "Development.Shake.Language.C.Rules" for examples of how to use a target
toolchain.

Windows is also a supported host operating system, see
"Development.Shake.Language.C.Host" for examples of how to target the host.

On Windows currently only the <http://www.mingw.org/ MinGW> toolchain is
supported.
-}

module Development.Shake.Language.C.Target.Windows (
    target
  , toolChain
) where

import           Data.Label (get, set)
import           Development.Shake
import           Development.Shake.Language.C.BuildFlags
import           Development.Shake.Language.C.Target
import           Development.Shake.Language.C.ToolChain

-- | Build target given an architecture.
target :: Arch -> Target
target = Target Windows (Platform "windows")

-- | Windows toolchain.
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
