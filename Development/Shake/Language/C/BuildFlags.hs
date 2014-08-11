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

module Development.Shake.Language.C.BuildFlags (
    BuildFlags
  , systemIncludes
  , userIncludes
  , defines
  , preprocessorFlags
  , compilerFlags
  , libraryPath
  , libraries
  , linkerFlags
  , localLibraries
  , archiverFlags
  , defineFlags
  , compilerFlagsFor
  , fromConfig
  , (>>>=)
  , append
  , prepend
) where

import           Control.Arrow
import           Control.Monad
import           Data.Monoid
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Development.Shake
import           Development.Shake.Language.C.Language (Language(..))
import           Development.Shake.Language.C.Util
import           Development.Shake.Language.C.Label

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
  } deriving (Show)

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

defineFlags :: BuildFlags -> [String]
defineFlags = concatMapFlag "-D"
            . map (\(a, b) -> maybe a (\b' -> a++"="++b') b)
            . get defines

compilerFlagsFor :: Maybe Language -> BuildFlags -> [String]
compilerFlagsFor lang = concat
                      . maybe (map snd . filter (isNothing.fst))
                              (mapMaybe . f) lang
                      . get compilerFlags
    where f _ (Nothing, x) = Just x
          f l (Just l', x) | l == l' = Just x
                           | otherwise = Nothing

fromConfig :: (String -> Action (Maybe String)) -> Action (BuildFlags -> BuildFlags)
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
    flags = words'
    paths = words'
    define [] = error "Empty preprocessor definition"
    define [k] = (k, Nothing)
    define [k,v] = (k, Just v)
    define (k:vs) = (k, Just (intercalate "=" vs))
    defines' = map (define . splitOn "=") . flags

-- | Utility function for composing functions in a monad.
(>>>=) :: Monad m => m (a -> b) -> m (b -> c) -> m (a -> c)
(>>>=) = liftM2 (>>>)
