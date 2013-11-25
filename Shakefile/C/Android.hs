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

module Shakefile.C.Android (
    platform
  , target
  , toolChain
  , standaloneToolChain
  , abiString
  , gnustl
  , native_app_glue
) where

import           Control.Lens hiding ((<.>))
import           Development.Shake.FilePath
import           Data.Version (Version(..), showVersion)
import           Shakefile.C
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree
import           Shakefile.Lens (append)
import qualified System.Info as System

platform :: Int -> Platform
platform apiVersion = Platform "android" (Version [apiVersion] [])

toolChainPrefix :: Target -> String
toolChainPrefix target =
    case target ^. targetArch of
        X86 _ -> "x86-"
        Arm _ -> "arm-linux-androideabi-"

osPrefix :: String
osPrefix = System.os ++ "-" ++ cpu
    where cpu = case System.arch of
                    "i386" -> "x86"
                    arch   -> arch

target :: Arch -> Platform -> Target
target arch = mkTarget arch "linux" "androideabi"

standaloneToolChain :: FilePath -> Target -> ToolChain
standaloneToolChain path target =
    prefix .~ Just path
  $ compilerCmd .~ mkTool "gcc"
  $ archiverCmd .~ mkTool "ar"
  $ linkerCmd .~ mkTool "g++"
  $ defaultToolChain
  where mkTool x = targetString target ++ "-" ++ x

toolChain :: FilePath -> ToolChainVariant -> Version -> Target -> ToolChain
toolChain ndk GCC (Version [4,7] []) target =
    variant .~ GCC
  $ prefix .~ Just (ndk </> "toolchains" </> tcPrefix ++ "4.7" </> "prebuilt" </> osPrefix)
  $ compilerCmd .~ mkTool "gcc"
  $ archiverCmd .~ mkTool "ar"
  $ linkerCmd .~ mkTool "g++"
  $ defaultBuildFlags .~ mkDefaultBuildFlags ndk target
  $ defaultToolChain
  where tcPrefix = toolChainPrefix target
        mkTool x = tcPrefix ++ x
toolChain _ variant version _ = error $ "Unknown tool chain variant " ++ show variant ++ " " ++ showVersion version

androidPlatformPrefix :: Target -> FilePath
androidPlatformPrefix target =
    platformName (target ^. targetPlatform)
     ++ "-"
     ++ show (head (versionBranch (platformVersion (target ^. targetPlatform))))
    </> "arch-" ++ archShortString (target ^. targetArch)

androidArchString :: Arch -> String
androidArchString (Arm Armv5) = "armv5te"
androidArchString (Arm Armv6) = "armv5te"
androidArchString (Arm Armv7) = "armv7-a"
androidArchString arch = archString arch

archCompilerFlags :: Arch -> [(Maybe Language, [String])]
archCompilerFlags (Arm Armv7) = [(Nothing, ["-mfloat-abi=softfp", "-mfpu=neon" {- vfpv3-d16 -}])]
archCompilerFlags (Arm _)     = [(Nothing, ["-mtune=xscale", "-msoft-float"])]
archCompilerFlags _ = []

archLinkerFlags :: Arch -> [String]
archLinkerFlags arch =
    case arch of
        Arm Armv7 -> common ++ ["-Wl,--fix-cortex-a8"]
        _         -> common
    where common = ["-Wl,--no-undefined", "-Wl,-z,relro", "-Wl,-z,now"]

mkDefaultBuildFlags :: FilePath -> Target -> BuildFlags -> BuildFlags
mkDefaultBuildFlags ndk target =
    append compilerFlags ([(Nothing, [sysroot, march])] ++ archCompilerFlags arch)
  . append compilerFlags ([(Nothing, [
      "-fpic"
    , "-ffunction-sections"
    , "-funwind-tables"
    , "-fstack-protector"
    , "-no-canonical-prefixes"])])
  . append linkerFlags ([sysroot, march] ++ archLinkerFlags arch)
  . append linkerFlags ["-no-canonical-prefixes"]
  . append archiverFlags ["-rs"]
  where
    arch = target ^. targetArch
    sysroot = "--sysroot=" ++ ndk </> "platforms" </> androidPlatformPrefix target
    march = "-march=" ++ androidArchString arch

abiString :: Arch -> String
abiString (Arm Armv5) = "armeabi"
abiString (Arm Armv6) = "armeabi"
abiString (Arm Armv7) = "armeabi-v7a"
abiString (X86 _)     = "x86"

native_app_glue :: FilePath -> SourceTree BuildFlags
native_app_glue ndk = SourceTree.flags (append systemIncludes [ndk </> "sources/android/native_app_glue"])
                        (SourceTree.files [ndk </> "sources/android/native_app_glue/android_native_app_glue.c"])

gnustl :: Linkage -> FilePath -> Target -> BuildFlags -> BuildFlags
gnustl linkage ndk target =
    append systemIncludes [stlPath </> "include", stlPath </> "libs" </> abi </> "include"]
  . append libraryPath [stlPath </> "libs" </> abi]
  . append libraries [lib]
    where stlPath = ndk </> "sources/cxx-stl/gnu-libstdc++/4.7"
          abi = abiString (target ^. targetArch)
          lib = case linkage of
                  Static -> "gnustl_static"
                  Shared -> "gnustl_shared"
