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
  , libppapi
  , libppapi_cpp
  , libnacl_io
  , libppapi_simple
) where

import           Control.Arrow ((>>>))
import           Control.Lens hiding ((<.>))
import           Development.Shake (Rules, need, system')
import           Development.Shake.FilePath
import           Data.Version (Version(..))
import           Shakefile.C
import           Shakefile.C.Host (OS(..))
import qualified Shakefile.C.Host as Host
import           Shakefile.Lens (append)

pepper :: Int -> Platform
pepper apiVersion = Platform "pepper" (Version [apiVersion] [])

canary :: Platform
canary = Platform "pepper" (Version [] ["canary"])

target :: Platform -> Target
target = mkTarget LLVM_IR "google" "native-client"

platformPrefix :: Target -> FilePath
platformPrefix target =
  platformName (target ^. targetPlatform)
  ++ "_"
  ++ case versionTags version of
      ["canary"] -> "canary"
      _          -> show $ head (versionBranch version)
  where platform = target ^. targetPlatform
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
          ++ buildFlags ^. archiverFlags
          ++ [output]
          ++ inputs
    system' (command (pnaclTool "ranlib") toolChain) [output]

data Config = Debug | Release deriving (Eq, Show)

toolChain :: FilePath -> Config -> Target -> ToolChain
toolChain sdk config target =
    variant .~ LLVM
  $ prefix .~ Just (platformDir </> "toolchain" </> hostString ++ "_" ++ "pnacl")
  $ compilerCmd .~ pnaclTool "clang"
  $ archiverCmd .~ pnaclTool "ar"
  $ archiver .~ archiver_
  $ linkerCmd .~ pnaclTool "clang++"
  $ linkResultFileName .~ (\linkResult ->
      case linkResult of
        Executable     -> (<.> "bc")
        SharedLibrary  -> ("lib"++) . (<.> "so")
        DynamicLibrary ->             (<.> "so"))
  $ defaultBuildFlags .~
      ( append userIncludes [platformDir </> "include"]
      . append libraryPath [platformDir </> "lib" </> "pnacl" </> show config] )
  $ defaultToolChain
  where platformDir = sdk </> platformPrefix target

-- | Finalize a bit code executable.
finalize :: ToolChain -> FilePath -> FilePath -> Rules FilePath
finalize toolChain input output = do
  let pexe = replaceExtension output "pexe"
  pexe ?=> \_ -> do
    need [input]
    system' (command (pnaclTool "finalize") toolChain)
            ["-o", pexe, input]
  return pexe

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
