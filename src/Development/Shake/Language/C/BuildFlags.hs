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

{-# LANGUAGE TemplateHaskell #-}

{-|
Description: Build flags record for building @C@ language projects

The `BuildFlags` record is an abstraction for various toolchain flags for
building executables and libraries from source files in a @C@-based language.
It's intended to be toolchain-independent, but currently there's a
bias towards binutils\/gcc/clang toolchains.
-}

module Development.Shake.Language.C.BuildFlags (
  -- * Source Language
    Language(..)
  -- * Build flags
  , BuildFlags
    -- Poor man's documentation for TH generated functions.
  , systemIncludes -- | System include directories, referenced by @#include \<...\>@ in code and usually passed to the compiler with the @-isystem@ flag.
  , userIncludes -- | User include directories, referenced by @#include "..."@ in code and usually passed to the compiler with the @-I@ flag.
  , defines -- | Preprocessor defines, a list of pairs of names with or without a value.
  , preprocessorFlags -- | Other preprocessor flags.
  , compilerFlags -- | Compiler flags, either generic ones or for a specific source 'Language'.
  , libraryPath -- | Linker search path for libraries.
  , libraries -- | List of libraries to link against. Note that you should use the library name without the @lib@ prefix and without extension.
  , linkerFlags -- | Flags passed to the linker.
  , localLibraries -- | Locally built static libraries to be linked against. See also the corresponding section in the <https://github.com/samplecount/shake-language-c/blob/master/docs/Manual.md#locally-built-libraries manual>.
  , archiverFlags -- | Flags passed to the object archiver.
  -- ** Utilities for toolchain writers
  , defineFlags
  , compilerFlagsFor
  -- ** Working with config files
  , fromConfig
  -- * Utilities
  , (>>>=)
  , append
  , prepend
) where

import           Control.Category ((>>>))
import           Control.Monad
import           Data.Char (isSpace)
import           Data.Default.Class (Default(..))
import           Data.Monoid
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake.Language.C.Language (Language(..))
import           Development.Shake.Language.C.Label
import           Development.Shake.Language.C.Util

{-| Record type for abstracting various toolchain command line flags.

`BuildFlags` is an instance of `Default`, you can create a default record with
`def`. `BuildFlags` is also an instance `Monoid`, you can create an empty record with
`mempty` and append flags with `mappend`. `def` and `mempty` are synonyms:

>>> (def :: BuildFlags) == (mempty :: BuildFlags)
True

Record accessors are `Data.Label.Mono.Lens`es from the
<https://hackage.haskell.org/package/fclabels fclabels> package, which
makes accessing and modifying record fields a bit more convenient.
@fclabels@ was chosen over <https://hackage.haskell.org/package/lens lens>
because it has far fewer dependencies, which is convenient when installing
the Shake build system in a per-project cabal sandbox. We might switch to
@lens@ when it gets included in the Haskell platform.

There are two convenience functions for working with `BuildFlags` record fields
containing lists of flags, `append` and `prepend`. Since most combinators in
this library expect a function @BuildFlags -> BuildFlags@, the following is a
common idiom:

@
buildFlags . append `systemIncludes` ["path"]
@

Note that when modifying the same record field, order of function composition
matters and you might want to use the arrow combinator '>>>' for appending in
source statement order:

>>> :{
  get systemIncludes
    $ append systemIncludes ["path1"] . append systemIncludes ["path2"]
    $ mempty
:}
["path2","path1"]

>>> :{
  get systemIncludes
    $ append systemIncludes ["path1"] >>> append systemIncludes ["path2"]
    $ mempty
:}
["path1","path2"]

See "Development.Shake.Language.C.Rules" for how to use 'BuildFlags' in build
product rules.
-}
data BuildFlags = BuildFlags {
    _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [(Maybe Language, [String])]
  , _libraryPath :: [FilePath]
  , _libraries :: [String]
  , _linkerFlags :: [String]
  -- This is needed for linking against local libraries built by shake (the linker `needs' its inputs).
  , _localLibraries :: [FilePath]
  , _archiverFlags :: [String]
  } deriving (Eq, Show)

mkLabel ''BuildFlags

defaultBuildFlags :: BuildFlags
defaultBuildFlags =
    BuildFlags {
        _systemIncludes = []
      , _userIncludes = []
      , _defines = []
      , _preprocessorFlags = []
      , _compilerFlags = []
      , _libraryPath = []
      , _libraries = []
      , _linkerFlags = []
      , _localLibraries = []
      , _archiverFlags = []
      }

instance Default BuildFlags where
  def = defaultBuildFlags

instance Monoid BuildFlags where
  mempty = defaultBuildFlags
  a `mappend` b =
      append systemIncludes    (get systemIncludes a)
    . append userIncludes      (get userIncludes a)
    . append defines           (get defines a)
    . append preprocessorFlags (get preprocessorFlags a)
    . append compilerFlags     (get compilerFlags a)
    . append libraryPath       (get libraryPath a)
    . append libraries         (get libraries a)
    . append linkerFlags       (get linkerFlags a)
    . append localLibraries    (get localLibraries a)
    . append archiverFlags     (get archiverFlags a)
    $ b

-- | Construct preprocessor flags from the 'defines' field of 'BuildFlags'.
defineFlags :: BuildFlags -> [String]
defineFlags = concatMapFlag "-D"
            . map (\(a, b) -> maybe a (\b' -> a++"="++b') b)
            . get defines

-- | Return a list of compiler flags for a specific source language.
compilerFlagsFor :: Maybe Language -> BuildFlags -> [String]
compilerFlagsFor lang = concat
                      . maybe (map snd . filter (isNothing.fst))
                              (mapMaybe . f) lang
                      . get compilerFlags
    where f _ (Nothing, x) = Just x
          f l (Just l', x) | l == l' = Just x
                           | otherwise = Nothing

-- | Construct a 'BuildFlags' modifier function from a config file.
--
-- See also "Development.Shake.Language.C.Config".
fromConfig :: (Functor m, Monad m) => (String -> m (Maybe String)) -> m (BuildFlags -> BuildFlags)
fromConfig getConfig = do
  let parseConfig parser = fmap (maybe [] parser) . getConfig . ("BuildFlags."++)

  config_systemIncludes    <- parseConfig paths "systemIncludes"
  config_userIncludes      <- parseConfig paths "userIncludes"
  config_defines           <- parseConfig defines' "defines"
  config_preprocessorFlags <- parseConfig flags "preprocessorFlags"
  config_compilerFlags     <- parseConfig ((:[]) . ((,)Nothing) . flags) "compilerFlags"
  config_compilerFlags_c   <- parseConfig ((:[]) . ((,)(Just C)) . flags) "compilerFlags.c"
  config_compilerFlags_cxx <- parseConfig ((:[]) . ((,)(Just Cpp)) . flags) "compilerFlags.cxx"
  config_libraryPath       <- parseConfig paths "libraryPath"
  config_libraries         <- parseConfig flags "libraries"
  config_linkerFlags       <- parseConfig flags "linkerFlags"
  config_localLibraries    <- parseConfig paths "localLibraries"
  config_archiverFlags     <- parseConfig flags "archiverFlags"

  return $  append systemIncludes    config_systemIncludes
          . append userIncludes      config_userIncludes
          . append defines           config_defines
          . append preprocessorFlags config_preprocessorFlags
          . append compilerFlags     (config_compilerFlags ++ config_compilerFlags_c ++ config_compilerFlags_cxx)
          . append libraryPath       config_libraryPath
          . append libraries         config_libraries
          . append linkerFlags       config_linkerFlags
          . append localLibraries    config_localLibraries
          . append archiverFlags     config_archiverFlags
  where
    flags = words' . dropWhile isSpace
    paths = words' . dropWhile isSpace
    define [] = error "Empty preprocessor definition"
    define [k] = (k, Nothing)
    define [k,v] = (k, Just v)
    define (k:vs) = (k, Just (intercalate "=" vs))
    defines' = map (define . splitOn "=") . flags

-- | Utility function for composing functions in a monad.
(>>>=) :: Monad m => m (a -> b) -> m (b -> c) -> m (a -> c)
(>>>=) = liftM2 (>>>)
