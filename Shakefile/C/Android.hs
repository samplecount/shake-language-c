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
  , abiString
  , gnustl
  , libcxx
  , native_app_glue
) where

import           Development.Shake.FilePath
import           Data.Version (Version(..), showVersion)
import           Shakefile.C
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree
import           Shakefile.Label (get, set, append)
import qualified System.Info as System

platform :: Int -> Platform
platform apiVersion = Platform "android" (Version [apiVersion] [])

toolChainPrefix :: Target -> String
toolChainPrefix target =
    case get targetArch target of
        X86 _ -> "x86-"
        Arm _ -> "arm-linux-androideabi-"
        arch  -> error $ "Unsupported Android target architecture " ++ archString arch

osPrefix :: String
osPrefix = System.os ++ "-" ++ cpu
    where cpu = case System.arch of
                    "i386" -> "x86"
                    arch   -> arch

target :: Arch -> Platform -> Target
target arch = mkTarget arch "linux" "androideabi"

toolChain :: FilePath -> (ToolChainVariant, Version) -> Target -> ToolChain
toolChain "" (_, _) _ = error "Empty NDK directory"
toolChain ndk (GCC, version) target =
    set variant GCC
  $ set toolDirectory (Just (ndk </> "toolchains"
                                 </> toolChainPrefix target ++ showVersion version
                                 </> "prebuilt"
                                 </> osPrefix
                                 </> "bin"))
  $ set toolPrefix (toolChainPrefix target)
  $ set compilerCommand "gcc"
  $ set archiverCommand "ar"
  $ set linkerCommand "g++"
  $ set defaultBuildFlags (mkDefaultBuildFlags ndk target)
  $ defaultToolChain
toolChain ndk (LLVM, version) target =
    set variant LLVM
  $ set toolDirectory (Just (ndk </> "toolchains"
                                 </> "llvm-" ++ showVersion version
                                 </> "prebuilt"
                                 </> osPrefix
                                 </> "bin"))
  $ set compilerCommand "clang"
  $ set archiverCommand "llvm-ar"
  $ set linkerCommand "clang++"
  $ set defaultBuildFlags (  mkDefaultBuildFlags ndk target
                           . append compilerFlags [(Nothing, ["-target", llvmTarget target]),
                                                   (Nothing, ["-gcc-toolchain", ndk </> "toolchains/arm-linux-androideabi-4.8/prebuilt" </> osPrefix])
                                                  ])
  $ defaultToolChain
  where
    llvmTarget target =
      case get targetArch target of
        Arm Armv5 -> "armv5te-none-linux-androideabi"
        Arm Armv7 -> "armv7-none-linux-androideabi"
        X86 I386 -> "i686-none-linux-android"
        _ -> error "Unsupported LLVM target architecture"

toolChain _ (variant, version) _ =
  error $ "Unknown tool chain variant "
        ++ show variant ++ " "
        ++ showVersion version

androidPlatformPrefix :: Target -> FilePath
androidPlatformPrefix target =
    platformName (get targetPlatform target)
     ++ "-"
     ++ show (head (versionBranch (platformVersion (get targetPlatform target))))
    </> "arch-" ++ archShortString (get targetArch target)

androidArchString :: Arch -> String
androidArchString (Arm Armv5) = "armv5te"
androidArchString (Arm Armv6) = "armv5te"
androidArchString (Arm Armv7) = "armv7-a"
androidArchString arch = archString arch

archCompilerFlags :: Arch -> [(Maybe Language, [String])]
archCompilerFlags (Arm Armv7) = [(Nothing, ["-mfloat-abi=softfp", "-mfpu=neon" {- vfpv3-d16 -}])]
archCompilerFlags (Arm _)     = [(Nothing, ["-mtune=xscale", "-msoft-float"])]
archCompilerFlags _           = []

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
  . append archiverFlags ["crs"]
  where
    arch = get targetArch target
    sysroot = "--sysroot=" ++ ndk </> "platforms" </> androidPlatformPrefix target
    march = "-march=" ++ androidArchString arch

abiString :: Arch -> String
abiString (Arm Armv5) = "armeabi"
abiString (Arm Armv6) = "armeabi"
abiString (Arm Armv7) = "armeabi-v7a"
abiString (X86 _)     = "x86"
abiString arch        = error $ "Unsupported Android target architecture " ++ archString arch

native_app_glue :: Monad m => FilePath -> SourceTree m BuildFlags
native_app_glue ndk = SourceTree.flags (append systemIncludes [ndk </> "sources/android/native_app_glue"])
                        (SourceTree.files [ndk </> "sources/android/native_app_glue/android_native_app_glue.c"])

gnustl :: Version -> Linkage -> FilePath -> Target -> BuildFlags -> BuildFlags
gnustl version linkage ndk target =
    append systemIncludes [stlPath </> "include", stlPath </> "libs" </> abi </> "include"]
  . append libraryPath [stlPath </> "libs" </> abi]
  . append libraries [lib]
    where stlPath = ndk </> "sources/cxx-stl/gnu-libstdc++" </> showVersion version
          abi = abiString (get targetArch target)
          lib = case linkage of
                  Static -> "gnustl_static"
                  Shared -> "gnustl_shared"

libcxx :: Linkage -> FilePath -> Target -> BuildFlags -> BuildFlags
libcxx linkage ndk target =
    append systemIncludes [ libcxxPath </> "libcxx" </> "include"
                          -- NOTE: libcxx needs to be first in include path!
                          , stlPath </> "gabi++" </> "include"
                          , ndk </> "sources" </> "android" </> "support" </> "include" ]
  . append libraryPath [libcxxPath </> "libs" </> abi]
  . append libraries [lib]
  . append compilerFlags [(Just Cpp, flags)]
  . append linkerFlags flags
    where stlPath = ndk </> "sources" </> "cxx-stl"
          libcxxPath = stlPath </> "llvm-libc++"
          abi = abiString (get targetArch target)
          lib = case linkage of
                  Static -> "libc++_static"
                  Shared -> "libc++_shared"
          flags = ["-stdlib=libc++"]
