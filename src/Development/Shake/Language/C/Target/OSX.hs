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

{-|
Description: Toolchain definitions and utilities for OSX and iOS

This module provides toolchain definitions and utilities for targeting OSX
and iOS. See "Development.Shake.Language.C.Rules" for examples of how to use a
target toolchain.

OSX is also a supported host operating system, see
"Development.Shake.Language.C.Host" for examples of how to target the host.
-}

module Development.Shake.Language.C.Target.OSX (
    DeveloperPath
  , getSDKRoot
  , macOSX
  , iPhoneOS
  , iPhoneSimulator
  , target
  , sdkVersion
  , toolChain
  , getPlatformVersions
  , macosx_version_min
  , iphoneos_version_min
  , universalBinary
) where

import           Control.Applicative
import           Data.List (stripPrefix)
import           Data.List.Split (splitOn)
import           Data.Maybe
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Development.Shake.Language.C.BuildFlags
import           Development.Shake.Language.C.Target
import           Development.Shake.Language.C.Label
import           Development.Shake.Language.C.ToolChain
import           System.Process (readProcess)
import           Text.Read (readMaybe)

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
    Arm Arm64  -> "arm64"
    _          -> error $ "Unsupported OSX target architecture " ++ show arch

archFlags :: Target -> [String]
archFlags t = ["-arch", archString (targetArch t)]

-- | Base path of development tools on OSX.
newtype DeveloperPath = DeveloperPath FilePath
                        deriving (Show)

-- | Get base path of development tools on OSX.
getSDKRoot :: Action DeveloperPath
getSDKRoot = liftIO $
  (DeveloperPath . head . splitOn "\n")
    <$> readProcess "xcode-select" ["--print-path"] ""

-- | Mac OSX platform.
macOSX :: Platform
macOSX = Platform "MacOSX"

-- | iOS platform.
iPhoneOS :: Platform
iPhoneOS = Platform "iPhoneOS"

-- | iOS simulator platform.
iPhoneSimulator :: Platform
iPhoneSimulator = Platform "iPhoneSimulator"

-- | Build target given a platform and an architecture.
target :: Platform -> Arch -> Target
target = Target OSX

sdkDirectory :: FilePath -> Platform -> FilePath
sdkDirectory sdkRoot platform =
      sdkRoot
  </> "Platforms"
  </> (platformName platform ++ ".platform")
  </> "Developer"
  </> "SDKs"

platformSDKPath :: FilePath -> Platform -> Version -> FilePath
platformSDKPath sdkRoot platform version =
      sdkDirectory sdkRoot platform
  </> platformName platform ++ showVersion version ++ ".sdk"

getPlatformVersionsWithRoot :: Platform -> DeveloperPath -> Action [Version]
getPlatformVersionsWithRoot platform (DeveloperPath sdkRoot) = do
  dirs <- getDirectoryDirs (sdkDirectory sdkRoot platform)
  case mapMaybe (\x -> parseVersion =<< stripPrefix name (dropExtension x)) dirs of
    [] -> error $ "No SDK found for " ++ name
    xs -> return xs
  where name = platformName platform
        parseVersion "" = Nothing
        parseVersion str =
          flip Version [] <$> mapM readMaybe (splitOn "." str)

-- | Return a list of available platform SDK versions.
--
-- For example in order to get the latest iOS SDK version:
--
-- > maximum <$> getPlatformVersions iPhoneOS
getPlatformVersions :: Platform -> Action [Version]
getPlatformVersions platform =
  getPlatformVersionsWithRoot platform =<< getSDKRoot

-- | SDK version given major and minor version numbers.
sdkVersion :: Int -> Int -> Version
sdkVersion major minor = Version [major, minor] []

-- | Construct an OSX or iOS toolchain.
toolChain :: DeveloperPath  -- ^ Developer tools base path, see `getSDKRoot`
          -> Version        -- ^ Target SDK version
          -> Target         -- ^ Build target, see `target`
          -> ToolChain      -- ^ Resulting toolchain
toolChain (DeveloperPath sdkRoot) version t =
    set variant LLVM
  $ set toolDirectory (Just (sdkRoot </> "Toolchains/XcodeDefault.xctoolchain/usr/bin"))
  $ set compilerCommand "clang"
  $ set archiverCommand "libtool"
  $ set archiver (\tc flags inputs output -> do
      need inputs
      command_ [] (tool tc archiverCommand)
        $  get archiverFlags flags
        ++ ["-static"]
        ++ ["-o", output]
        ++ inputs
    )
  $ set linkerCommand "clang++"
  $ set linker (\lr tc ->
      case lr of
        Executable      -> defaultLinker tc
        SharedLibrary   -> defaultLinker tc . prepend linkerFlags ["-dynamiclib"]
        LoadableLibrary -> defaultLinker tc . prepend linkerFlags ["-bundle"]
    )
  $ set defaultBuildFlags
      ( return $
        append preprocessorFlags [ "-isysroot", sysRoot ]
      . append compilerFlags [(Nothing, archFlags t)]
      . append linkerFlags (archFlags t ++ [ "-isysroot", sysRoot ]) )
  $ defaultToolChain
  where sysRoot = platformSDKPath sdkRoot (targetPlatform t) version

-- | Specify the @-mmacosx-version-min@ compiler flag.
macosx_version_min :: Version -> BuildFlags -> BuildFlags
macosx_version_min version =
  append compilerFlags [(Nothing, ["-mmacosx-version-min=" ++ showVersion version])]

-- | Specify the @-miphoneos-version-min@ compiler flag.
iphoneos_version_min :: Version -> BuildFlags -> BuildFlags
iphoneos_version_min version =
  append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ showVersion version])]

-- | Create a universal binary a list of input files.
--
-- Calls <http://www.manpages.info/macosx/lipo.1.html lipo> with the @-create@ option.
universalBinary :: [FilePath] -- ^ Input files, can be executables, dynamic libraries or static archives but should be all of the same type
                -> FilePath   -- ^ Output file path
                -> Action ()
universalBinary inputs output = do
  need inputs
  command_ [] "lipo" $ ["-create", "-output", output] ++ inputs
