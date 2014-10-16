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

{-|
Description: Toolchain definitions and utilities for Android

This module provides toolchain definitions and utilities for targeting Android.
See "Development.Shake.Language.C.Rules" for examples of how to use a target
toolchain.
-}
module Development.Shake.Language.C.Target.Android (
    target
  , sdkVersion
  , toolChain
  , abiString
  , gnustl
  , libcxx
  , native_app_glue
) where

import           Control.Arrow
import           Development.Shake.FilePath
import           Data.Version (Version(..), showVersion)
import           Development.Shake.Language.C.BuildFlags
import           Development.Shake.Language.C.Target
import           Development.Shake.Language.C.Label
import           Development.Shake.Language.C.Language
import           Development.Shake.Language.C.ToolChain
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

-- | Android target for architecture.
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

-- | Construct a version record from an integral Android SDK version.
--
-- prop> sdkVersion 19 == Version [19] []
sdkVersion :: Int -> Version
sdkVersion n = Version [n] []

-- | Construct an Android toolchain.
toolChain :: FilePath                     -- ^ NDK source directory
          -> Version                      -- ^ SDK version, see `sdkVersion`
          -> (ToolChainVariant, Version)  -- ^ Toolchain variant and version
          -> Target                       -- ^ Build target, see `target`
          -> ToolChain                    -- ^ Resulting toolchain
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
  $ set defaultBuildFlags (return $ mkDefaultBuildFlags ndk version (targetArch t))
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
  $ set defaultBuildFlags (return $
    let gcc_toolchain = ndk </> "toolchains/arm-linux-androideabi-4.8/prebuilt" </> osPrefix
        flags = ["-target", llvmTarget t, "-gcc-toolchain", gcc_toolchain]
    in  mkDefaultBuildFlags ndk version (targetArch t)
      . append compilerFlags [(Nothing, flags)]
      . append linkerFlags flags
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
  error $ "Unknown toolchain variant "
        ++ show tcVariant ++ " "
        ++ showVersion tcVersion

-- | Valid Android ABI identifier for the given architecture.
abiString :: Arch -> String
abiString (Arm Armv5) = "armeabi"
abiString (Arm Armv6) = "armeabi"
abiString (Arm Armv7) = "armeabi-v7a"
abiString (X86 _)     = "x86"
abiString arch        = unsupportedArch arch

-- | Source paths and build flags for the @native_app_glue@ module.
native_app_glue :: FilePath -- ^ NDK source directory
                -> ([FilePath], BuildFlags -> BuildFlags)
native_app_glue ndk =
  ( [ndk </> "sources/android/native_app_glue/android_native_app_glue.c"]
  , append systemIncludes [ndk </> "sources/android/native_app_glue"] )

-- | Build flags for building with and linking against the GNU @gnustl@ standard C++ library.
gnustl :: Version                     -- ^ GNU STL version
       -> Linkage                     -- ^ `Static` or `Shared`
       -> FilePath                    -- ^ NDK source directory
       -> Target                      -- ^ Build target, see `target`
       -> (BuildFlags -> BuildFlags)  -- ^ 'BuildFlags' modification function
gnustl version linkage ndk t =
    append systemIncludes [stlPath </> "include", stlPath </> "libs" </> abi </> "include"]
  . append libraryPath [stlPath </> "libs" </> abi]
  . append libraries [lib]
    where stlPath = ndk </> "sources/cxx-stl/gnu-libstdc++" </> showVersion version
          abi = abiString (targetArch t)
          lib = case linkage of
                  Static -> "gnustl_static"
                  Shared -> "gnustl_shared"

-- | Build flags for building with and linking against the LLVM @libc++@ standard C++ library.
libcxx :: Linkage                     -- ^ `Static` or `Shared`
       -> FilePath                    -- ^ NDK source directory
       -> Target                      -- ^ Build target, see `target`
       -> (BuildFlags -> BuildFlags)  -- ^ 'BuildFlags' modification function
libcxx linkage ndk t =
    append systemIncludes [ libcxxPath </> "libcxx" </> "include"
                          -- NOTE: libcxx needs to be first in include path!
                          , stlPath </> "gabi++" </> "include"
                          , ndk </> "sources" </> "android" </> "support" </> "include" ]
  . append compilerFlags [(Just Cpp, ["-stdlib=libc++"])]
  . append libraryPath [libcxxPath </> "libs" </> abi]
  . prepend libraries [lib]
    where stlPath = ndk </> "sources" </> "cxx-stl"
          libcxxPath = stlPath </> "llvm-libc++"
          abi = abiString (targetArch t)
          lib = case linkage of
                  Static -> "c++_static"
                  Shared -> "c++_shared"
