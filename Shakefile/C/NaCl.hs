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
  , toolChain
) where

import           Control.Lens hiding ((<.>))
import           Development.Shake (need, system')
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

toolChain :: FilePath -> Target -> ToolChain
toolChain sdk target =
    variant .~ LLVM
  $ prefix .~ Just (sdk </> platformPrefix target </> "toolchain" </> hostString ++ "_" ++ "pnacl")
  $ compilerCmd .~ pnaclTool "clang"
  $ archiverCmd .~ pnaclTool "ar"
  $ archiver .~ archiver_
  $ linkerCmd .~ pnaclTool "clang++"
  $ defaultToolChain
