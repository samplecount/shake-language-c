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

module Shakefile.C.NaCl (
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
import           Development.Shake (Rules, need, system', writeFileLines)
import           Development.Shake.FilePath
import           Data.Version (Version(..))
import           Shakefile.C hiding (Arch)
import qualified Shakefile.C as C
import           Shakefile.C.Host (OS(..))
import qualified Shakefile.C.Host as Host
import           Shakefile.Label (append, get, set)

pepper :: Int -> Platform
pepper apiVersion = Platform "pepper" (Version [apiVersion] [])

canary :: Platform
canary = Platform "pepper" (Version [] ["canary"])

target :: Platform -> Target
target = mkTarget LLVM_IR "google" "native-client"

platformPrefix :: Target -> FilePath
platformPrefix target =
  platformName platform
  ++ "_"
  ++ case versionTags version of
      ["canary"] -> "canary"
      _          -> show $ head (versionBranch version)
  where platform = get targetPlatform target
        version = platformVersion platform

hostString :: String
hostString =
  case Host.os of
    Linux   -> "linux"
    OSX     -> "mac"
    Windows -> "win"

pnaclTool :: String -> String
pnaclTool = ("pnacl-"++)

archiver_ :: Archiver
archiver_ toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
          $  ["cr"]
          ++ get archiverFlags buildFlags
          ++ [output]
          ++ inputs
    system' (command (pnaclTool "ranlib") toolChain) [output]

data Config = Debug | Release deriving (Eq, Show)

toolChain :: FilePath -> Config -> Target -> ToolChain
toolChain sdk config target =
    set variant LLVM
  $ set prefix (Just (platformDir </> "toolchain" </> hostString ++ "_" ++ "pnacl"))
  $ set compilerCmd (pnaclTool "clang")
  $ set archiverCmd (pnaclTool "ar")
  $ set archiver archiver_
  $ set linkerCmd (pnaclTool "clang++")
  $ set linkResultFileName (\linkResult ->
      case linkResult of
        Executable     -> (<.> "bc")
        SharedLibrary  -> ("lib"++) . (<.> "so")
        DynamicLibrary ->             (<.> "so"))
  $ set defaultBuildFlags
      ( append userIncludes [platformDir </> "include"]
      . append libraryPath [platformDir </> "lib" </> "pnacl" </> show config] )
  $ defaultToolChain
  where platformDir = sdk </> platformPrefix target

-- | Finalize a bit code executable.
finalize :: ToolChain -> FilePath -> FilePath -> Rules FilePath
finalize toolChain input output = do
  output ?=> \_ -> do
    need [input]
    system' (command (pnaclTool "finalize") toolChain)
            ["-o", output, input]
  return output

-- | Translate bit code to native code.
translate :: ToolChain -> C.Arch -> FilePath -> FilePath -> Rules FilePath
translate toolChain arch input output = do
  let archString =
        case arch of
          X86 I686   -> "i686"
          X86 X86_64 -> "x86-64"
          Arm Armv7  -> "armv7"
          _ -> error $ "Unsupported architecture: " ++ show arch
  output ?=> \_ -> do
    need [input]
    system' (command (pnaclTool "finalize") toolChain)
            ["-arch", archString, "-o", output, input]
  return output

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

mk_nmf :: [(Arch, FilePath)] -> FilePath -> Rules FilePath
mk_nmf inputs output = do
  output ?=> \_ -> do
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
  return output
  where
    program (PNaCl, input) = [
        "    \"portable\": {"
      , "      \"pnacl-translate\": {"
      , "        \"url\": \"" ++ makeRelative (takeDirectory output) input ++ "\""
      , "      }"
      ]
    program (NaCl arch, input) =
      let archString = case arch of
                        X86 X86_64 -> "x86_64"
                        X86 _      -> "x86_32"
                        Arm _      -> "arm"
      in [
           "    \"" ++ archString ++ "\": {"
         , "      \"url\": \"" ++ makeRelative (takeDirectory output) input ++ "\""
         , "    }"
         ]
