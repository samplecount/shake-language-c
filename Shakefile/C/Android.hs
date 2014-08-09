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
    target
  , apiVersion
  , toolChain
  , abiString
  , gnustl
  , libcxx
  , native_app_glue
) where

import           Control.Arrow
import           Development.Shake.FilePath
import           Data.Version (Version(..), showVersion)
import           Shakefile.C
import           Shakefile.Label (set, append)
import qualified System.Info as System

unsupportedArch :: Arch -> a
unsupportedArch arch = error $ "Unsupported Android target architecture " ++ archString arch

toolChainPrefix :: Target -> String
toolChainPrefix x =
    case targetArch x of
        X86 _ -> "x86-"
        Arm _ -> "arm-linux-androideabi-"
        arch  -> unsupportedArch arch

osPrefix :: String
osPrefix = System.os ++ "-" ++ cpu
    where cpu = case System.arch of
                    "i386" -> "x86"
                    arch   -> arch

target :: Arch -> Target
target = Target Android (Platform "android")

mkDefaultBuildFlags :: FilePath -> Version -> Arch -> BuildFlags -> BuildFlags
mkDefaultBuildFlags ndk version arch =
      append compilerFlags [(Nothing, [sysroot, march])]
  >>> append compilerFlags (archCompilerFlags arch)
  >>> append compilerFlags [(Nothing, [
        "-fpic"
      , "-ffunction-sections"
      , "-funwind-tables"
      , "-fstack-protector"
      , "-no-canonical-prefixes"])]
  >>> append linkerFlags [sysroot, march]
  >>> append linkerFlags (archLinkerFlags arch)
  >>> append linkerFlags ["-Wl,--no-undefined", "-Wl,-z,relro", "-Wl,-z,now"]
  >>> append linkerFlags ["-no-canonical-prefixes"]
  >>> append archiverFlags ["crs"]
  where
    sysroot =    "--sysroot="
              ++ ndk
              </> "platforms"
              </> "android-" ++ show (head (versionBranch version))
              </> "arch-" ++ case arch of
                              (X86 _) -> "x86"
                              (Arm _) -> "arm"
                              _       -> unsupportedArch arch
    march = "-march=" ++ case arch of
                          (Arm Armv5) -> "armv5te"
                          (Arm Armv6) -> "armv5te"
                          (Arm Armv7) -> "armv7-a"
                          _           -> archString arch
    archCompilerFlags (Arm Armv7) = [(Nothing, ["-mfloat-abi=softfp", "-mfpu=neon" {- vfpv3-d16 -}])]
    archCompilerFlags (Arm _)     = [(Nothing, ["-mtune=xscale", "-msoft-float"])]
    archCompilerFlags _           = []
    archLinkerFlags (Arm Armv7)   = ["-Wl,--fix-cortex-a8"]
    archLinkerFlags _             = []

apiVersion :: Int -> Version
apiVersion n = Version [n] []

toolChain :: FilePath -> Version -> (ToolChainVariant, Version) -> Target -> ToolChain
toolChain "" _ (_, _) _ = error "Empty NDK directory"
toolChain ndk version (GCC, tcVersion) t =
    set variant GCC
  $ set toolDirectory (Just (ndk </> "toolchains"
                                 </> toolChainPrefix t ++ showVersion tcVersion
                                 </> "prebuilt"
                                 </> osPrefix
                                 </> "bin"))
  $ set toolPrefix (toolChainPrefix t)
  $ set compilerCommand "gcc"
  $ set archiverCommand "ar"
  $ set linkerCommand "g++"
  $ set defaultBuildFlags (mkDefaultBuildFlags ndk version (targetArch t))
  $ defaultToolChain
toolChain ndk version (LLVM, tcVersion) t =
    set variant LLVM
  $ set toolDirectory (Just (ndk </> "toolchains"
                                 </> "llvm-" ++ showVersion tcVersion
                                 </> "prebuilt"
                                 </> osPrefix
                                 </> "bin"))
  $ set compilerCommand "clang"
  $ set archiverCommand "llvm-ar"
  $ set linkerCommand "clang++"
  $ set defaultBuildFlags (
        mkDefaultBuildFlags ndk version (targetArch t)
      . append compilerFlags [
            (Nothing, ["-target", llvmTarget t])
          , (Nothing, [ "-gcc-toolchain"
                      , ndk </> "toolchains/arm-linux-androideabi-4.8/prebuilt" </> osPrefix ])
          ]
    )
  $ defaultToolChain
  where
    llvmTarget x =
      case targetArch x of
        Arm Armv5 -> "armv5te-none-linux-androideabi"
        Arm Armv7 -> "armv7-none-linux-androideabi"
        X86 I386  -> "i686-none-linux-android"
        arch      -> unsupportedArch arch
toolChain _ _ (tcVariant, tcVersion) _ =
  error $ "Unknown tool chain variant "
        ++ show tcVariant ++ " "
        ++ showVersion tcVersion

abiString :: Arch -> String
abiString (Arm Armv5) = "armeabi"
abiString (Arm Armv6) = "armeabi"
abiString (Arm Armv7) = "armeabi-v7a"
abiString (X86 _)     = "x86"
abiString arch        = unsupportedArch arch

native_app_glue :: FilePath -> ([FilePath], BuildFlags -> BuildFlags)
native_app_glue ndk =
  ( [ndk </> "sources/android/native_app_glue/android_native_app_glue.c"]
  , append systemIncludes [ndk </> "sources/android/native_app_glue"] )

gnustl :: Version -> Linkage -> FilePath -> Target -> BuildFlags -> BuildFlags
gnustl version linkage ndk t =
    append systemIncludes [stlPath </> "include", stlPath </> "libs" </> abi </> "include"]
  . append libraryPath [stlPath </> "libs" </> abi]
  . append libraries [lib]
    where stlPath = ndk </> "sources/cxx-stl/gnu-libstdc++" </> showVersion version
          abi = abiString (targetArch t)
          lib = case linkage of
                  Static -> "gnustl_static"
                  Shared -> "gnustl_shared"

libcxx :: Linkage -> FilePath -> Target -> BuildFlags -> BuildFlags
libcxx linkage ndk t =
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
          abi = abiString (targetArch t)
          lib = case linkage of
                  Static -> "libc++_static"
                  Shared -> "libc++_shared"
          flags = ["-stdlib=libc++"]
