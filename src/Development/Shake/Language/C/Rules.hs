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

{-|
Description: High-level Shake rules

This module provides a few high-level rules for building executables and
libraries. Below is an example that builds both a static library and an executable. See
"Development.Shake.Language.C.ToolChain" for examples of toolchain definitions.

> let toolChain = ...
> lib <- staticLibrary toolChain "libexample.a" (pure mempty) (pure ["example_lib.c"])
> exe <- staticLibrary toolChain ("example" <.> exe) (pure mempty) (pure ["example_exe.c"])
> want [lib, exe]

Sometimes you want to structure your project in a set of static libraries that
are later linked into one or more executables. For Shake to recognise the
libraries as dependencies of the executable you need to add them to the
`localLibraries` field of the `BuildFlags` record:

> let toolChain = ...
>     buildFlags = ...
> lib <- staticLibrary toolChain "libexample.a"
>         (pure buildFlags)
>         (pure ["example_lib.c"])
> exe <- executable toolChain ("example" <.> exe)
>         (pure $ buildFlags . append localLibraries [lib])
>         (pure ["example_exe.c"])
> want [exe]

The rule functions expect their arguments in the 'Action' monad in order to be
able to derive them from side-effecting configuration actions. For example it
can be useful to determine certain toolchain settings either from the
environment, or from configuration files. Using "Control.Applicative" we could
write:

> Android.toolChain
>   <$> getEnvWithDefault
>         (error "ANDROID_NDK is undefined")
>         "ANDROID_NDK"
>   <*> pure (Android.sdkVersion 9)
>   <*> pure (LLVM, Version [3,4] [])
>   <*> pure (Android.target (Arm Armv7))
-}

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

-- | Shake rule for building an executable.
executable :: Action ToolChain -- ^ Action returning a target 'ToolChain'
           -> FilePath -- ^ Output file
           -> Action (BuildFlags -> BuildFlags) -- ^ Action returning a 'BuildFlags' modifier
           -> Action [FilePath] -- ^ Action returning a list of input source files
           -> Rules FilePath -- ^ Rule returning the output file path
executable toolChain = buildProduct (flip (get linker) Executable) toolChain

-- | Shake rule for building a static library.
staticLibrary :: Action ToolChain -- ^ Action returning a target 'ToolChain'
              -> FilePath -- ^ Output file
              -> Action (BuildFlags -> BuildFlags) -- ^ Action returning a 'BuildFlags' modifier
              -> Action [FilePath] -- ^ Action returning a list of input source files
              -> Rules FilePath -- ^ Rule returning the output file path
staticLibrary toolChain = buildProduct (get archiver) toolChain

-- | Shake rule for building a shared (dynamically linked) library.
sharedLibrary :: Action ToolChain -- ^ Action returning a target 'ToolChain'
              -> FilePath -- ^ Output file
              -> Action (BuildFlags -> BuildFlags) -- ^ Action returning a 'BuildFlags' modifier
              -> Action [FilePath] -- ^ Action returning a list of input source files
              -> Rules FilePath -- ^ Rule returning the output file path
sharedLibrary toolChain = buildProduct (flip (get linker) SharedLibrary) toolChain

-- | Shake rule for building a dynamically loadable library.
loadableLibrary :: Action ToolChain -- ^ Action returning a target 'ToolChain'
                -> FilePath -- ^ Output file
                -> Action (BuildFlags -> BuildFlags) -- ^ Action returning a 'BuildFlags' modifier
                -> Action [FilePath] -- ^ Action returning a list of input source files
                -> Rules FilePath -- ^ Rule returning the output file path
loadableLibrary toolChain = buildProduct (flip (get linker) LoadableLibrary) toolChain
