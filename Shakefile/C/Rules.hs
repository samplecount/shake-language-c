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

mkObjectsDir :: FilePath -> FilePath
mkObjectsDir path = takeDirectory path </> map tr (takeFileName path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

buildProduct :: Linker
             -> ToolChain
             -> SourceTree Action BuildFlags
             -> FilePath
             -> Rules FilePath
buildProduct link toolChain sources resultPath = do
    let objectsDir = mkObjectsDir resultPath
        sources' = SourceTree.flags (get ToolChain.defaultBuildFlags toolChain) sources
    objects <- forM (SourceTree.flatten sources') $ \(getBuildFlags, (src, deps)) -> do
        let obj = objectsDir </> makeRelative "/" (src <.> "o")
        obj ?=> \_ -> do
          buildFlags <- getBuildFlags
          objectFile toolChain (buildFlags BuildFlags.defaultBuildFlags) src deps obj
        return obj
    resultPath ?=> \_ -> do
      buildFlags <- SourceTree.collect sources'
      link toolChain (buildFlags BuildFlags.defaultBuildFlags) objects resultPath
    return resultPath

-- | Rule for building an executable.
executable :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
executable toolChain resultPath sources =
    buildProduct
        (get linker toolChain Executable)
        toolChain
        sources
        (get linkResultFileName toolChain Executable resultPath)

-- | Rule for building a static library.
staticLibrary :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
staticLibrary toolChain resultPath sources =
    buildProduct
        (get archiver toolChain)
        toolChain
        sources
        (get archiveFileName toolChain resultPath)

-- | Rule for building a shared library.
sharedLibrary :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
sharedLibrary toolChain resultPath sources =
    buildProduct
        (get linker toolChain SharedLibrary)
        toolChain
        sources
        (get linkResultFileName toolChain SharedLibrary resultPath)

-- | Rule for building a dynamic library.
dynamicLibrary :: ToolChain -> FilePath -> SourceTree Action BuildFlags -> Rules FilePath
dynamicLibrary toolChain resultPath sources =
    buildProduct
        (get linker toolChain DynamicLibrary)
        toolChain
        sources
        (get linkResultFileName toolChain DynamicLibrary resultPath)
