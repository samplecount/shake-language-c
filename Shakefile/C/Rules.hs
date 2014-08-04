-- Copyright 2012-2014 Samplecount S.L.
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

module Shakefile.C.Rules (
    executable
  , staticLibrary
  , sharedLibrary
  , dynamicLibrary
) where

import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Shakefile.C.BuildFlags as BuildFlags
import           Shakefile.C.ToolChain as ToolChain
import           Shakefile.C.Util
import           Shakefile.Label (get)
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree

applyDefaultBuildFlags :: Monad m => ToolChain -> SourceTree m BuildFlags -> SourceTree m BuildFlags
applyDefaultBuildFlags toolChain sources =
  SourceTree.flags (get ToolChain.defaultBuildFlags toolChain) sources

objects :: ToolChain
        -> SourceTree Action BuildFlags
        -> FilePath
        -> Rules [FilePath]
objects toolChain sources objectsDir = do
    forM (SourceTree.flatten (applyDefaultBuildFlags toolChain sources)) $ \(getBuildFlags, (src, deps)) -> do
        let obj = objectsDir </> makeRelative "/" (src <.> "o")
        obj ?=> \_ -> do
          need deps
          buildFlags <- getBuildFlags
          (get compiler toolChain)
            toolChain
            (buildFlags BuildFlags.defaultBuildFlags)
            src
            obj
        return obj

mkObjectsDir :: FilePath -> FilePath
mkObjectsDir path = takeDirectory path </> map tr (takeFileName path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

-- | Rule for building a static library.
staticLibrary :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
staticLibrary toolChain result sources = do
  objs <- objects toolChain sources (mkObjectsDir result)
  result ?=> \_ -> do
    buildFlags <- SourceTree.collect $ applyDefaultBuildFlags toolChain sources
    (get archiver toolChain)
      toolChain
      (buildFlags BuildFlags.defaultBuildFlags)
      objs
      result
  return result

linkProduct :: LinkResult
            -> ToolChain
            -> FilePath
            -> SourceTree Action BuildFlags
            -> Rules FilePath
linkProduct linkResult toolChain result sources = do
  objs <- objects toolChain sources (mkObjectsDir result)
  result ?=> \_ -> do
    buildFlags <- SourceTree.collect $ applyDefaultBuildFlags toolChain sources
    ((get linker toolChain) linkResult)
      toolChain
      (buildFlags BuildFlags.defaultBuildFlags)
      objs
      result
  return result

-- | Rule for building an executable.
executable :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
executable = linkProduct Executable

-- | Rule for building a shared library.
sharedLibrary :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
sharedLibrary = linkProduct SharedLibrary

-- | Rule for building a dynamic library.
dynamicLibrary :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
dynamicLibrary = linkProduct DynamicLibrary
