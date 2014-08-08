-- Copyright 2012-2013 Samplecount S.L.
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

module Shakefile.C.PkgConfig (
    Options(..)
  , defaultOptions
  , pkgConfigWithOptions
  , pkgConfig
  , fromConfig
  , fromConfigWithOptions
) where

import Control.Applicative
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import Development.Shake
import Shakefile.C ( BuildFlags
                   , compilerFlags
                   , libraries
                   , libraryPath
                   , linkerFlags
                   , systemIncludes
                   , userIncludes )
import Shakefile.Label (append)
import Shakefile.C.Util (words')
import System.FilePath (searchPathSeparator)

-- ====================================================================
-- PkgConfig

-- TODO:
--  * Use parsec or attoparsec for more robust parser
--  * Parse preprocessor defines
--  * Parse framework path (-F) and -framework flags

parseCflags :: [String] -> (BuildFlags -> BuildFlags)
parseCflags [] = id
parseCflags (x:xs)
  | isPrefixOf "-I" x = parseCflags xs . append systemIncludes [drop 2 x]
  | isPrefixOf "-i" x = parseCflags xs . append userIncludes [drop 2 x]
  | otherwise = append compilerFlags [(Nothing, [x])]

parseLibs :: [String] -> (BuildFlags -> BuildFlags)
parseLibs [] = id
parseLibs (x:xs)
  | isPrefixOf "-l" x = parseLibs xs . append libraries [drop 2 x]
  | isPrefixOf "-L" x = parseLibs xs . append libraryPath [drop 2 x]
  | otherwise = parseLibs xs . append linkerFlags [x]

parseFlags :: String -> [String]
parseFlags = words' . head . lines

data Options = Options {
    searchPath :: Maybe [FilePath]
  , static :: Bool
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
    searchPath = Nothing
  , static = False
  }

pkgConfigWithOptions :: Options -> String -> Action (BuildFlags -> BuildFlags)
pkgConfigWithOptions options pkg = do
  env <- case searchPath options of
    Nothing -> return []
    Just path -> do
      env <- addEnv [("PKG_CONFIG_PATH", intercalate [searchPathSeparator] path)]
      return [env]
  let flags = if static options then ["--static"] else []
      pkgconfig which = command ([Traced ""] ++ env) "pkg-config" (flags ++ ["--" ++ which, pkg])
  Stdout cflags <- pkgconfig "cflags"
  Stdout libs <- pkgconfig "libs"
  return (  parseCflags (parseFlags cflags)
          . parseLibs (parseFlags libs) )

pkgConfig :: String -> Action (BuildFlags -> BuildFlags)
pkgConfig = pkgConfigWithOptions defaultOptions

fromConfigWithOptions :: Options -> (String -> Action (Maybe String)) -> Action (BuildFlags -> BuildFlags)
fromConfigWithOptions initialOptions cfg = do
  config_searchPath <- fmap words' <$> cfg "PkgConfig.options.searchPath"
  config_static <- fmap (bool . words) <$> cfg "PkgConfig.options.static"
  config_packages <- fmap words <$> cfg "PkgConfig.packages"
  let options = initialOptions {
      searchPath = maybe (searchPath initialOptions) Just config_searchPath,
      static = maybe (static initialOptions) id config_static
    }
  flags <- mapM (pkgConfigWithOptions options)
                (maybe [] id config_packages)
  return $ foldl (.) id $ flags
  where
    bool (x:_) = map toLower x == "true"
    bool _ = False

fromConfig :: (String -> Action (Maybe String)) -> Action (BuildFlags -> BuildFlags)
fromConfig = fromConfigWithOptions defaultOptions
