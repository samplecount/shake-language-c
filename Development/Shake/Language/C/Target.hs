-- Copyright 2012-2014 Samplecount S.L.
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

module Development.Shake.Language.C.Target (
    OS(..)
  , Platform(..)
  , Arch(..)
  , archString
  , ArmVersion(..)
  , X86Version(..)
  , Target(..)
  , ToBuildPrefix(..)
) where

import           Data.Char (toLower)
import           Development.Shake.FilePath

data OS =
    Android
  | Linux
  | OSX
  | Pepper
  | Windows
  deriving (Eq, Ord, Show)

data Platform = Platform {
    platformName :: String
  } deriving (Eq, Show)

data X86Version =
    I386
  | I686
  | X86_64
  deriving (Eq, Show)

data ArmVersion =
    Armv5
  | Armv6
  | Armv7
  | Armv7s
  deriving (Eq, Show)

data Arch =
    X86 X86Version
  | Arm ArmVersion
  | LLVM_IR
  deriving (Eq, Show)

archString :: Arch -> String
archString arch =
  case arch of
    X86 I386   -> "i386"
    X86 I686   -> "i686"
    X86 X86_64 -> "x86_64"
    Arm Armv5  -> "armv5"
    Arm Armv6  -> "armv6"
    Arm Armv7  -> "armv7"
    Arm Armv7s -> "armv7s"
    LLVM_IR    -> "llvm_ir"

data Target = Target {
    targetOS :: OS
  , targetPlatform :: Platform
  , targetArch :: Arch
  } deriving (Show)

class ToBuildPrefix a where
  toBuildPrefix :: a -> FilePath

instance ToBuildPrefix Platform where
  toBuildPrefix = map toLower . platformName

instance ToBuildPrefix Target where
   toBuildPrefix target =
          toBuildPrefix (targetPlatform target)
      </> archString (targetArch target)
