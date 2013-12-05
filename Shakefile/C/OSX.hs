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

module Shakefile.C.OSX (
    DeveloperPath
  , getDeveloperPath
  , getSystemVersion
  , macOSX
  , iPhoneOS
  , iPhoneSimulator
  , target
  , getDefaultToolChain
  , toolChain
  , macosx_version_min
  , macosx_version_target
  , iphoneos_version_min
  , iphoneos_version_target
  , universalBinary
) where

import           Control.Applicative ((<$>))
import           Data.List.Split (splitOn)
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Shakefile.C
import           Shakefile.Label (append, get, prepend, set)
import           System.Process (readProcess)

osxArchiver :: Archiver
osxArchiver toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
          $  get archiverFlags buildFlags
          ++ ["-static"]
          ++ ["-o", output]
          ++ inputs

archFlags :: Target -> [String]
archFlags target = ["-arch", archString (get targetArch target)]

osxLinker :: LinkResult -> Linker
osxLinker link toolChain =
    case link of
        Executable     -> defaultLinker toolChain
        SharedLibrary  -> defaultLinker toolChain . prepend linkerFlags ["-dynamiclib"]
        DynamicLibrary -> defaultLinker toolChain . prepend linkerFlags ["-bundle"]

newtype DeveloperPath = DeveloperPath { developerPath :: FilePath }

-- | Get base path of development tools on OSX.
getDeveloperPath :: IO DeveloperPath
getDeveloperPath =
  (DeveloperPath . head . splitOn "\n")
    <$> readProcess "xcode-select" ["--print-path"] ""

platformDeveloperPath :: DeveloperPath -> String -> FilePath
platformDeveloperPath developer platform =
  developerPath developer </> "Platforms" </> (platform ++ ".platform") </> "Developer"

macOSX :: Version -> Platform
macOSX = Platform "MacOSX"

iPhoneOS :: Version -> Platform
iPhoneOS = Platform "iPhoneOS"

iPhoneSimulator :: Version -> Platform
iPhoneSimulator = Platform "iPhoneSimulator"

target :: Arch -> Platform -> Target
target arch = mkTarget arch "apple" "darwin10"

platformSDKPath :: DeveloperPath -> Platform -> FilePath
platformSDKPath developer platform =
      platformDeveloperPath developer name
  </> "SDKs"
  </> (name ++ showVersion (platformVersion platform) ++ ".sdk")
  where name = platformName platform

-- | Get OSX system version (first two digits).
getSystemVersion :: IO Version
getSystemVersion =
  flip Version []
    <$> (map read . take 2 . splitOn ".")
    <$> readProcess "sw_vers" ["-productVersion"] ""

getDefaultToolChain :: IO (Target, ToolChain)
getDefaultToolChain = do
    myVersion <- getSystemVersion
    myDeveloperPath <- getDeveloperPath
    let defaultTarget = target (X86 X86_64) (macOSX myVersion)
    return (defaultTarget, toolChain myDeveloperPath defaultTarget)

toolChain :: DeveloperPath -> Target -> ToolChain
toolChain developer target =
    set variant LLVM
  $ set prefix (Just (developerPath developer </> "Toolchains/XcodeDefault.xctoolchain/usr"))
  $ set compilerCmd "clang"
  $ set archiverCmd "libtool"
  $ set archiver osxArchiver
  $ set linkerCmd "clang++"
  $ set linker osxLinker
  $ set linkResultFileName (\linkResult ->
      case linkResult of
        Executable     -> id
        SharedLibrary  -> ("lib"++) . (<.> "dylib")
        DynamicLibrary ->             (<.> "dylib"))
  $ set defaultBuildFlags ( append preprocessorFlags [ "-isysroot", sysRoot ]
                          . append compilerFlags [(Nothing, archFlags target)]
                          . append linkerFlags (archFlags target ++ [ "-isysroot", sysRoot ]) )
  $ defaultToolChain
  where sysRoot = platformSDKPath developer (get targetPlatform target)

macosx_version_min :: Version -> BuildFlags -> BuildFlags
macosx_version_min version = append compilerFlags [(Nothing, ["-mmacosx-version-min=" ++ showVersion version])]

macosx_version_target :: Target -> BuildFlags -> BuildFlags
macosx_version_target = macosx_version_min . platformVersion . get targetPlatform

iphoneos_version_min :: Version -> BuildFlags -> BuildFlags
iphoneos_version_min version = append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ showVersion version])]

iphoneos_version_target :: Target -> BuildFlags -> BuildFlags
iphoneos_version_target = iphoneos_version_min . platformVersion . get targetPlatform

universalBinary :: [FilePath] -> FilePath -> Rules FilePath
universalBinary inputs output = do
    output ?=> \_ -> do
        need inputs
        system' "lipo" $ ["-create", "-output", output] ++ inputs
    return output
