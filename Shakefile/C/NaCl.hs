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
    Linux   -> error "hostString: is \"linux\" correct"
    OSX     -> "mac"
    Windows -> error "hostString: is \"win\" correct?"

toolChain :: FilePath -> Target -> ToolChain
toolChain sdk target =
    variant .~ LLVM
  $ prefix .~ Just (sdk </> platformPrefix target </> "toolchain" </> hostString ++ "_" ++ "pnacl")
  $ compilerCmd .~ mkTool "clang"
  $ archiverCmd .~ mkTool "ar"
  $ linkerCmd .~ mkTool "clang++"
  $ defaultBuildFlags .~ append archiverFlags ["r"]
  $ defaultToolChain
  where mkTool x = "pnacl-" ++ x
