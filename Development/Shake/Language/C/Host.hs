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

module Development.Shake.Language.C.Host (
    OS(..)
  , os
  , executableExtension
  , sharedLibraryExtension
  , loadableLibraryExtension
  , defaultToolChain
) where

import           Development.Shake (Action)
import           Development.Shake.Language.C (Target, ToolChain)
import qualified Development.Shake.Language.C.Linux as Linux
import qualified Development.Shake.Language.C.OSX as OSX
import qualified Development.Shake.Language.C.Windows as Windows
import qualified System.Info as System
import           System.IO.Unsafe (unsafePerformIO)

-- | Host operating system.
data OS =
    Linux
  | OSX
  | Windows
  deriving (Eq, Ord, Show)

-- | This host's operating system.
os :: OS
os =
  case System.os of
    "darwin"  -> OSX
    "mingw32" -> Windows
    "linux"   -> Linux
    _         -> error $ "Unknown host operating system: " ++ System.os

-- | File extension for executables.
executableExtension :: String
executableExtension =
  case os of
    Windows -> "exe"
    _       -> ""

-- | File extension for dynamic shared libraries.
sharedLibraryExtension :: String
sharedLibraryExtension =
  case os of
    Linux   -> "so"
    OSX     -> "dylib"
    Windows -> "dll"

-- | File extension for dynamic loadable libraries.
loadableLibraryExtension :: String
loadableLibraryExtension =
  case os of
    Linux   -> "so"
    OSX     -> "bundle"
    Windows -> "dll"

-- | Get host's default tool chain.
defaultToolChain :: (Target, Action ToolChain)
{-# NOINLINE defaultToolChain #-}
defaultToolChain = unsafePerformIO $ do
  -- The assumption here is that target and toolchain don't change while the program is running.
  case os of
    Linux -> Linux.getDefaultToolChain
    OSX -> OSX.getDefaultToolChain
    Windows -> Windows.getDefaultToolChain
    -- _ -> error $ "No default toolchain for this operating system (" ++ show os ++ ")"