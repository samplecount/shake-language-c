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
  , Target(..)
  , ToBuildPrefix(..)
) where

import           Data.Char (toLower)
import           Development.Shake.FilePath

-- $setup
-- >>> :load Development.Shake.Language.C.Target.OSX
-- >>> :module -Development.Shake.Language.C.Target.OSX
-- >>> import Development.Shake.Language.C.Target

-- | Target operating system.
data OS =
    Android -- ^ Google Android
  | Linux   -- ^ GNU Linux
  | OSX     -- ^ Apple Mac OSX and iOS
  | Pepper  -- ^ Google Portable Native Client (PNaCl)
  | Windows -- ^ Microsoft Windows
  | None    -- ^ No OS (bare metal)
  deriving (Eq, Ord, Show)

-- | Target platform.
--
-- Basically just a platform identifier string. Use `toBuildPrefix` to convert a platform to a file path prefix that can be used in Shake rules.
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
  | Arm64
  deriving (Eq, Show)

-- | Target architecture.
--
-- Use `toBuildPrefix` to convert an architecture to a short, more or less canonical file path prefix that can be used in Shake rules.
data Arch =
    X86 X86Version  -- ^ Intel @x86@ architecture
  | Arm ArmVersion  -- ^ Arm architecture
  | LLVM_IR         -- ^ LLVM intermediate representation, used by `Pepper` (PNaCl)
  deriving (Eq, Show)

-- | Compilation target triple consisting of operating system, platform and architecture.
--
-- Use `toBuildPrefix` to convert a target to a file path prefix that can be used in Shake rules. The prefix is of the form
--
-- > <platform>/<architecture>
--
-- For example:
--
-- >>> import qualified Development.Shake.Language.C.Target.OSX as OSX
-- >>> toBuildPrefix $ OSX.target OSX.iPhoneOS (Arm Armv7)
-- "iphoneos/armv7"
data Target = Target {
    targetOS :: OS              -- ^ Target operating system
  , targetPlatform :: Platform  -- ^ Target platform
  , targetArch :: Arch          -- ^ Target architecture
  } deriving (Show)

-- | Convert a value to a build directory prefix.
--
-- The idea is that several such values can be combined to form more complex build directory hierarchies. This can be important for disambiguating build product paths in Shake rules.
class ToBuildPrefix a where
  -- | Convert a value to a (unique) build directory prefix.
  toBuildPrefix :: a -> FilePath

instance ToBuildPrefix Platform where
  toBuildPrefix = map toLower . platformName

instance ToBuildPrefix Arch where
  toBuildPrefix arch =
    case arch of
      X86 version -> map toLower (show version)
      Arm version -> map toLower (show version)
      _           -> map toLower (show arch)

instance ToBuildPrefix Target where
   toBuildPrefix target =
          toBuildPrefix (targetPlatform target)
      </> toBuildPrefix (targetArch target)
