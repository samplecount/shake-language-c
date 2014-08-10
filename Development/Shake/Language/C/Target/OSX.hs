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
  , getDefaultToolChain
  , macosx_version_min
  , iphoneos_version_min
  , universalBinary
) where

import           Control.Applicative hiding ((*>))
import           Data.List (stripPrefix)
import           Data.List.Split (splitOn)
import           Data.Maybe
import           Data.Version (Version(..), showVersion)
import           Development.Shake as Shake
import           Development.Shake.FilePath
import           Development.Shake.Language.C
import           Development.Shake.Language.C.Label (append, get, prepend, set)
import           System.Process (readProcess)
import           Text.Read (readEither)

archFlags :: Target -> [String]
archFlags t = ["-arch", archString (targetArch t)]

newtype DeveloperPath = DeveloperPath FilePath
                        deriving (Show)

-- | Get base path of development tools on OSX.
getSDKRoot :: Action DeveloperPath
getSDKRoot = liftIO $
  (DeveloperPath . head . splitOn "\n")
    <$> readProcess "xcode-select" ["--print-path"] ""

macOSX :: Platform
macOSX = Platform "MacOSX"

iPhoneOS :: Platform
iPhoneOS = Platform "iPhoneOS"

iPhoneSimulator :: Platform
iPhoneSimulator = Platform "iPhoneSimulator"

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

getPlatformVersions :: Platform -> DeveloperPath -> Action [Version]
getPlatformVersions platform (DeveloperPath sdkRoot) = do
  dirs <- getDirectoryDirs (sdkDirectory sdkRoot platform)
  case mapMaybe (stripPrefix name) dirs of
    [] -> error $ "OSX: No SDK found for " ++ name
    xs -> return [ flip Version []
                 . map (either error id . readEither)
                 . splitOn "."
                 . dropExtension $ x
                 | x <- xs ]
  where name = platformName platform

-- | Get OSX system version (first two digits).
systemVersion :: Action Version
systemVersion = liftIO $
  flip Version []
    <$> (map read . take 2 . splitOn ".")
    <$> readProcess "sw_vers" ["-productVersion"] ""

getDefaultToolChain :: IO (Target, Action ToolChain)
getDefaultToolChain = do
    let defaultTarget = target macOSX (X86 X86_64)
    return ( defaultTarget
           , toolChain
               <$> getSDKRoot
               <*> systemVersion
               <*> pure defaultTarget )

sdkVersion :: Int -> Int -> Version
sdkVersion major minor = Version [major, minor] []

toolChain :: DeveloperPath -> Version -> Target -> ToolChain
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

macosx_version_min :: Version -> BuildFlags -> BuildFlags
macosx_version_min version =
  append compilerFlags [(Nothing, ["-mmacosx-version-min=" ++ showVersion version])]

iphoneos_version_min :: Version -> BuildFlags -> BuildFlags
iphoneos_version_min version =
  append compilerFlags [(Nothing, ["-miphoneos-version-min=" ++ showVersion version])]

universalBinary :: [FilePath] -> FilePath -> Action ()
universalBinary inputs output = do
  need inputs
  command_ [] "lipo" $ ["-create", "-output", output] ++ inputs
