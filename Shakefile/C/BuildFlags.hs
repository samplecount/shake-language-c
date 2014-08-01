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

module Shakefile.C.BuildFlags (
    BuildFlags
  , defaultBuildFlags
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
) where

import           Data.Maybe
import           Shakefile.C.Language (Language)
import           Shakefile.C.Util (concatMapFlag)
import           Shakefile.Label (get, mkLabel)

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
