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

module Development.Shake.Language.C.Rules (
    executable
  , staticLibrary
  , sharedLibrary
  , loadableLibrary
) where

import Data.Monoid (mempty)
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Language.C.BuildFlags as BuildFlags
import Development.Shake.Language.C.ToolChain as ToolChain
import Development.Shake.Language.C.Label (get)

mkObjectsDir :: FilePath -> FilePath
mkObjectsDir path = takeDirectory path </> map tr (takeFileName path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

buildProduct :: (ToolChain -> Linker)
             -> Action ToolChain
             -> FilePath
             -> Action (BuildFlags -> BuildFlags)
             -> Action [FilePath]
             -> Rules FilePath
buildProduct getLinker getToolChain result getBuildFlags getSources = do
  let objectsDir = mkObjectsDir result
  cachedObjects <- newCache $ \() -> do
    sources <- getSources
    return $ [ objectsDir </> makeRelative "/" (src <.> if isAbsolute src then "abs.o" else "rel.o")
             | src <- sources ]
  cachedToolChain <- newCache $ \() -> getToolChain
  cachedBuildFlags <- newCache $ \() -> do
    tc <- cachedToolChain ()
    f1 <- get ToolChain.defaultBuildFlags tc
    f2 <- getBuildFlags
    return $ f2 . f1 $ mempty
  result *> \_ -> do
    tc <- cachedToolChain ()
    flags <- cachedBuildFlags ()
    objs <- cachedObjects ()
    need objs
    (getLinker tc)
      tc
      flags
      objs
      result
  (dropTrailingPathSeparator objectsDir ++ "//*.o") *> \obj -> do
    tc <- cachedToolChain ()
    flags <- cachedBuildFlags ()
    -- Compute source file name from object file name
    -- Using getSources here would result in a dependency of every object file on the list of sources, leading to unnecessary rebuilds.
    let src = case splitExtension $ dropExtension $ makeRelative objectsDir obj of
                (x, ".abs") -> "/" </> x -- Source file had absolute path
                (x, ".rel") -> x
                (_, ext)    -> error $ "BUG: Unexpected object file extension " ++ ext
    (get compiler tc)
      tc
      flags
      src
      obj
  return result

-- TODO: The following result type would be more composable, e.g. allowing for further build product processing:
-- Rules (FilePath -> Action ())
-- See https://github.com/samplecount/shake-language-c/issues/15

executable :: Action ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
executable toolChain = buildProduct (flip (get linker) Executable) toolChain

staticLibrary :: Action ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
staticLibrary toolChain = buildProduct (get archiver) toolChain

sharedLibrary :: Action ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
sharedLibrary toolChain = buildProduct (flip (get linker) SharedLibrary) toolChain

loadableLibrary :: Action ToolChain -> FilePath -> Action (BuildFlags -> BuildFlags) -> Action [FilePath] -> Rules FilePath
loadableLibrary toolChain = buildProduct (flip (get linker) LoadableLibrary) toolChain
