-- Copyright 2013 Samplecount S.L.
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
Description: Toolchain definitions and utilities for Google Pepper and Portable Native Client

This module provides toolchain definitions and utilities for targeting Google
Pepper and Portable Native Client (PNaCl). Arguably it should be renamed
appropriately. See "Development.Shake.Language.C.Rules" for examples of how to
use a target toolchain.
-}
module Development.Shake.Language.C.Target.NaCl (
    pepper
  , canary
  , target
  , Config(..)
  , toolChain
  , finalize
  , translate
  , Executable(..)
  , Program(..)
  , mk_nmf
) where

import           Development.Shake
import           Development.Shake.FilePath
import           Data.Version (Version(..))
import           Development.Shake.Language.C hiding (Arch)
import qualified Development.Shake.Language.C.Target as C
import           Development.Shake.Language.C.ToolChain
import qualified Development.Shake.Language.C.Host as Host
import           Development.Shake.Language.C.Label

-- | Stable /Pepper/ API version.
pepper :: Int -> Version
pepper apiVersion = Version [apiVersion] []

-- | Unstable /Pepper Canary/ API version.
canary :: Version
canary = Version [] ["canary"]

-- | Pepper target.
--
-- `LLVM_IR` (PNaCl) is the only supported target architecture at the moment.
target :: Target
target = Target Pepper (Platform "pepper") LLVM_IR

hostString :: String
hostString =
  case Host.os of
    Host.Linux   -> "linux"
    Host.OSX     -> "mac"
    Host.Windows -> "win"

-- | Pepper build configuration.
--
-- This is used to select the respective library versions when linking.
data Config = Debug | Release deriving (Eq, Show)

-- | Construct Pepper toolchain.
toolChain :: FilePath   -- ^ Pepper SDK directory (@nacl_sdk@)
          -> Version    -- ^ Pepper API version, see `pepper` and `canary`
          -> Config     -- ^ Build configuration for linked libraries
          -> Target     -- ^ Target, see `target`
          -> ToolChain  -- ^ Resulting toolchain
toolChain sdk sdkVersion config t =
    set variant LLVM
  $ set toolDirectory (Just (platformDir </> "toolchain" </> hostString ++ "_" ++ "pnacl" </> "bin"))
  $ set toolPrefix "pnacl-"
  $ set compilerCommand "clang"
  $ set archiverCommand "ar"
  $ set archiver (\tc flags inputs output -> do
      need inputs
      command_ [] (tool tc archiverCommand)
        $  ["cr"]
        ++ get archiverFlags flags
        ++ [output]
        ++ inputs
      command_ [] (toolFromString tc "ranlib") [output]
    )
  $ set linkerCommand "clang++"
  $ set defaultBuildFlags
      ( return $
        append systemIncludes [includeDir]
      . append userIncludes [includeDir]
      . append systemIncludes [includeDir </> "pnacl"]
      . append libraryPath [platformDir </> "lib" </> "pnacl" </> show config] )
  $ defaultToolChain
  where
    platformDir =
          sdk
      </> platformName (targetPlatform t)
          ++ "_"
          ++ case versionTags sdkVersion of
              ["canary"] -> "canary"
              _          -> show $ head (versionBranch sdkVersion)
    includeDir = platformDir </> "include"

-- | Finalize a bit code executable.
finalize :: ToolChain -- ^ Toolchain, see `toolChain`
         -> FilePath  -- ^ Bit code input executable
         -> FilePath  -- ^ Finalised bit code output executable
         -> Action ()
finalize tc input output = do
  need [input]
  command_ [] (toolFromString tc "finalize")
              ["-o", output, input]

-- | Translate bit code to native code.
translate :: ToolChain -> C.Arch -> FilePath -> FilePath -> Action ()
translate tc arch input output = do
  let archName =
        case arch of
          X86 I686   -> "i686"
          X86 X86_64 -> "x86-64"
          Arm Armv7  -> "armv7"
          _ -> error $ "Unsupported architecture: " ++ show arch
  need [input]
  command_ [] (toolFromString tc "finalize")
              ["-arch", archName, "-o", output, input]

-- | Executable specification for Native Client Manifest (nmf) files.
data Executable = Executable {
    executablePath :: FilePath      -- ^ Relative path to executable.
  , optimizationLevel :: Maybe Int  -- ^ Optional optimization level.
  } deriving (Eq, Show)

-- | Program specification for Native Client Manifest (nmf) files.
data Program = Program {
    pnaclTranslate :: Executable    -- ^ Release executable (pexe)
  , pnaclDebug :: Maybe Executable  -- ^ Executable used when debugging (bit code).
  } deriving (Eq, Show)

-- | Create Native Client Manifest (nmf) files.
--
-- This file is needed for serving PNaCl outside the Google Play store. See the native client <https://developer.chrome.com/native-client/reference/nacl-manifest-format documentation> for more information on the file format.
mk_nmf :: Program  -- ^ Program specification
       -> FilePath    -- ^ Output file
       -> Action ()
mk_nmf program output = do
  need $  [executablePath (pnaclTranslate program)]
       ++ maybe [] ((:[]).executablePath) (pnaclDebug program)
  writeFileChanged output . unlines $ [
      "{"
    , "  \"program\": {"
    , "    \"portable\": {"
    ]
    ++ entry "pnacl-translate" (pnaclTranslate program)
    ++ maybe [] (\e -> [","] ++ entry "pnacl-debug" e) (pnaclDebug program)
    ++
    [ "    }"
    , "  }"
    , "}"
    ]
  where
    entry what exe = [
        "      \"" ++ what ++ "\": {"
      , "        \"url\": \"" ++ makeRelative (takeDirectory output) (executablePath exe) ++ "\""
      ] ++ maybe [] (\n ->
       ["      , \"optlevel\": " ++ show n]) (optimizationLevel exe)
        ++
      [ "      }" ]
