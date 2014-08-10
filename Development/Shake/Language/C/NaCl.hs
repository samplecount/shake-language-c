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

module Development.Shake.Language.C.NaCl (
    pepper
  , canary
  , target
  , Config(..)
  , toolChain
  , finalize
  , translate
  , libppapi
  , libppapi_cpp
  , libnacl_io
  , libppapi_simple
  , Arch(..)
  , mk_nmf
) where

import           Data.List (intercalate)
import           Development.Shake
import           Development.Shake.FilePath
import           Data.Version (Version(..))
import           Development.Shake.Language.C hiding (Arch)
import qualified Development.Shake.Language.C as C
import qualified Development.Shake.Language.C.Host as Host
import           Development.Shake.Language.C.Label (append, get, set)

pepper :: Int -> Version
pepper apiVersion = Version [apiVersion] []

canary :: Version
canary = Version [] ["canary"]

target :: Target
target = Target Pepper (Platform "pepper") LLVM_IR

hostString :: String
hostString =
  case Host.os of
    Host.Linux   -> "linux"
    Host.OSX     -> "mac"
    Host.Windows -> "win"

data Config = Debug | Release deriving (Eq, Show)

toolChain :: FilePath -> Version -> Config -> Target -> ToolChain
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
finalize :: ToolChain -> FilePath -> FilePath -> Action ()
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

-- | Link against the Pepper C API library.
libppapi :: BuildFlags -> BuildFlags
libppapi = append libraries ["ppapi"]

-- | Link against the Pepper C++ API library.
libppapi_cpp :: BuildFlags -> BuildFlags
libppapi_cpp = append libraries ["ppapi_cpp"]

-- | Link against libnacl_io.
libnacl_io :: BuildFlags -> BuildFlags
libnacl_io = append libraries ["nacl_io"]

-- | Link against the Simple Pepper C API library.
libppapi_simple :: BuildFlags -> BuildFlags
libppapi_simple = append libraries ["ppapi_simple"]

data Arch =
    PNaCl
  | NaCl C.Arch
  deriving (Eq, Show)

mk_nmf :: [(Arch, FilePath)] -> FilePath -> Action ()
mk_nmf inputs output = do
  need $ map snd inputs
  writeFileLines output $ [
      "{"
    , "  \"program\": {"
    ]
    ++ intercalate [","] (map program inputs) ++
    [ "    }"
    , "  }"
    , "}"
    ]
  where
    program (PNaCl, input) = [
        "    \"portable\": {"
      , "      \"pnacl-translate\": {"
      , "        \"url\": \"" ++ makeRelative (takeDirectory output) input ++ "\""
      , "      }"
      ]
    program (NaCl arch, input) =
      let archName = case arch of
                        X86 X86_64 -> "x86_64"
                        X86 _      -> "x86_32"
                        Arm _      -> "arm"
                        _          -> error $ "mk_nmf: Unsupported architecture " ++ show arch
      in [
           "    \"" ++ archName ++ "\": {"
         , "      \"url\": \"" ++ makeRelative (takeDirectory output) input ++ "\""
         , "    }"
         ]