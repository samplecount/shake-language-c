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
  , ArmVersion(..)
  , X86Version(..)
  , Arch(..)
  , archString
  , Target(..)
  , ToBuildPrefix(..)
) where

import           Data.Char (toLower)
import           Development.Shake.FilePath

-- | Target operating system.
data OS =
    Android -- ^ Google Android
  | Linux   -- ^ GNU Linux
  | OSX     -- ^ Apple Mac OSX and iOS
  | Pepper  -- ^ Google Portable Native Client (PNaCl)
  | Windows -- ^ Microsoft Windows
  deriving (Eq, Ord, Show)

-- | Target platform.
--
-- Basically just a platform identifier string.
data Platform = Platform {
    platformName :: String
  } deriving (Eq, Show)

-- | `X86` architecture version.
data X86Version =
    I386    -- ^ @i386@, 32-bit architecture without @SSE@
  | I686    -- ^ @i686@, 32-bit architecture with @SSE@ (/Pentium-Pro/)
  | X86_64  -- ^ @x86_64@, 64-bit architecture
  deriving (Eq, Show)

-- | `Arm` architecture version.
data ArmVersion =
    Armv5
  | Armv6
  | Armv7
  | Armv7s
  deriving (Eq, Show)

-- | Target architecture.
data Arch =
    X86 X86Version  -- ^ Intel @x86@ architecture
  | Arm ArmVersion  -- ^ Arm architecture
  | LLVM_IR         -- ^ LLVM intermediate representation, used by `Pepper` (PNaCl)
  deriving (Eq, Show)

-- | Architecture short string.
--
-- Mainly useful for constructing build output directories.
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

-- | Compilation target triple consisting of operating system, platform and architecture.
data Target = Target {
    targetOS :: OS              -- ^ Target operating system
  , targetPlatform :: Platform  -- ^ Target platform
  , targetArch :: Arch          -- ^ Target architecture
  } deriving (Show)

-- | Convert a value to a build directory prefix.
--
-- The idea is that several such values can be combined to form more complex build directory hierarchies. This can be important for disambiguating build product paths in Shake rules.
class ToBuildPrefix a where
  toBuildPrefix :: a -> FilePath

instance ToBuildPrefix Platform where
  toBuildPrefix = map toLower . platformName

instance ToBuildPrefix Arch where
  toBuildPrefix = archString

instance ToBuildPrefix Target where
   toBuildPrefix target =
          toBuildPrefix (targetPlatform target)
      </> toBuildPrefix (targetArch target)
