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
  , toolChain_MacOSX
  , toolChain_MacOSX_gcc
  , toolChain_IOS
  , toolChain_IOS_gcc
  , toolChain_IOS_Simulator
  , toolChain_IOS_Simulator_gcc
  , universalBinary
) where

import           Control.Applicative ((<$>))
import           Control.Lens hiding ((<.>))
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Version (Version(..), showVersion)
import           Shakefile.C
import           Shakefile.Lens (append, prepend)
import           System.Process (readProcess)

osxArchiver :: Archiver
osxArchiver toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
          $  buildFlags ^. archiverFlags
          ++ ["-static"]
          ++ ["-o", output]
          ++ inputs

archFlags :: Target -> [String]
archFlags target = ["-arch", (archString $ target ^. targetArch)]

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

osxLinkResultFileName :: LinkResult -> String -> FilePath
osxLinkResultFileName Executable = id
osxLinkResultFileName SharedLibrary = ("lib"++) . (<.> "dylib")
osxLinkResultFileName DynamicLibrary =            (<.> "dylib")

getDefaultToolChain :: IO (Target, ToolChain)
getDefaultToolChain = do
    version <- getSystemVersion
    developerPath <- getDeveloperPath
    let defaultTarget = target (X86 X86_64) (macOSX version)
    return (defaultTarget, toolChain_MacOSX defaultTarget developerPath)

mkDefaultBuildFlags :: Target -> DeveloperPath -> BuildFlags -> BuildFlags
mkDefaultBuildFlags target developer =
    append preprocessorFlags [ "-isysroot", sysRoot ]
  . append compilerFlags [(Nothing, archFlags target)]
  . append linkerFlags (archFlags target ++ [ "-isysroot", sysRoot ])
  where sysRoot = platformSDKPath developer (target ^. targetPlatform)

toolChain_MacOSX :: Target -> DeveloperPath -> ToolChain
toolChain_MacOSX target developer =
    variant .~ LLVM
  $ prefix .~ Just (developerPath developer </> "Toolchains/XcodeDefault.xctoolchain/usr")
  $ compilerCmd .~ "clang"
  $ archiverCmd .~ "libtool"
  $ archiver .~ osxArchiver
  $ linkerCmd .~ "clang++"
  $ linker .~ osxLinker
  $ linkResultFileName .~ osxLinkResultFileName
  $ defaultBuildFlags .~ ( append compilerFlags [(Nothing, ["-mmacosx-version-min=" ++ showVersion (platformVersion (target ^. targetPlatform))])]
                         . mkDefaultBuildFlags target developer )
  $ defaultToolChain

toolChain_MacOSX_gcc :: Target -> DeveloperPath -> ToolChain
toolChain_MacOSX_gcc target developer =
    variant .~ GCC
  $ compilerCmd .~ "gcc"
  $ linkerCmd .~ "g++"
  $ toolChain_MacOSX target developer

iosMinVersion :: String
iosMinVersion = "5.0" -- Required for C++11
--iosMinVersion = "40200"

toolChain_IOS :: Target -> DeveloperPath -> ToolChain
toolChain_IOS target developer =
    defaultBuildFlags .~ ( append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ iosMinVersion])]
                         . mkDefaultBuildFlags target developer )
  $ toolChain_MacOSX target developer

toolChain_IOS_gcc :: Target -> DeveloperPath -> ToolChain
toolChain_IOS_gcc target developer =
    variant .~ GCC
  $ prefix .~ Just (developerPath developer </> "Platforms/iPhoneOS.platform/Developer/usr")
  $ compilerCmd .~ "llvm-gcc"
  $ linkerCmd .~ "llvm-g++"
  $ toolChain_IOS target developer

toolChain_IOS_Simulator :: Target -> DeveloperPath -> ToolChain
toolChain_IOS_Simulator target developer =
    defaultBuildFlags .~ ( append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ iosMinVersion])]
                         . mkDefaultBuildFlags target developer )
  $ toolChain_MacOSX target developer

toolChain_IOS_Simulator_gcc :: Target -> DeveloperPath -> ToolChain
toolChain_IOS_Simulator_gcc target developer =
    variant .~ GCC
  $ prefix .~ Just (developerPath developer </> "Platforms/iPhoneSimulator.platform/Developer/usr")
  $ toolChain_IOS_gcc target developer

universalBinary :: [FilePath] -> FilePath -> Rules FilePath
universalBinary inputs output = do
    output ?=> \_ -> do
        need inputs
        system' "lipo" $ ["-create", "-output", output] ++ inputs
    return output
