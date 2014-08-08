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

import           Development.Shake
import           Development.Shake.FilePath
import           Shakefile.C.BuildFlags as BuildFlags
import           Shakefile.C.ToolChain as ToolChain
import           Shakefile.Label (get)

mkObjectsDir :: FilePath -> FilePath
mkObjectsDir path = takeDirectory path </> map tr (takeFileName path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

buildProduct :: Linker
             -> ToolChain
             -> FilePath
             -> Action (BuildFlags -> BuildFlags)
             -> Action [FilePath]
             -> Rules FilePath
buildProduct link toolChain result getBuildFlags getSources = do
  let objectsDir = mkObjectsDir result
  cachedObjects <- newCache $ \() -> do
    sources <- getSources
    return $ [ objectsDir </> makeRelative "/" (src <.> if isAbsolute src then "abs.o" else "rel.o")
             | src <- sources ]
  cachedBuildFlags <- newCache $ \() -> getBuildFlags
  result *> \_ -> do
    buildFlags <- cachedBuildFlags ()
    objs <- cachedObjects ()
    need objs
    link
      toolChain
      (buildFlags . get ToolChain.defaultBuildFlags toolChain $ BuildFlags.defaultBuildFlags)
      objs
      result
  (dropTrailingPathSeparator objectsDir ++ "//*.o") *> \obj -> do
    buildFlags <- cachedBuildFlags ()
    -- Compute source file name from object file name
    let src = case splitExtension $ dropExtension $ makeRelative objectsDir obj of
                (x, ".abs") -> "/" </> x -- Source file had absolute path
                (x, ".rel") -> x
                (_, ext)    -> error $ "BUG: Unexpected object file extension " ++ ext
    (get compiler toolChain)
      toolChain
      (buildFlags . get ToolChain.defaultBuildFlags toolChain $ BuildFlags.defaultBuildFlags)
      src
      obj
  return result

executable :: ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
executable toolChain = buildProduct (get linker toolChain Executable) toolChain

staticLibrary :: ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
staticLibrary toolChain = buildProduct (get archiver toolChain) toolChain

sharedLibrary :: ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
sharedLibrary toolChain = buildProduct (get linker toolChain SharedLibrary) toolChain

dynamicLibrary :: ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
dynamicLibrary toolChain = buildProduct (get linker toolChain DynamicLibrary) toolChain
