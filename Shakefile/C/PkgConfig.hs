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
) where

import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Shakefile.C ( BuildFlags
                   , compilerFlags
                   , libraries
                   , libraryPath
                   , linkerFlags
                   , systemIncludes
                   , userIncludes )
import Shakefile.Label (append)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath (searchPathSeparator)
import System.Process (readProcessWithExitCode)
import System.SetEnv (setEnv, unsetEnv)

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
parseFlags = unescape . words . head . lines
  where
    escape = "\\"
    isEscaped = isSuffixOf escape
    dropEscape = reverse . drop (length escape) . reverse
    unescape [] = []
    unescape [x] = [if isEscaped x then dropEscape x else x]
    unescape (x1:x2:xs)
      | isEscaped x1 = unescape ((dropEscape x1 ++ x2):xs)
      | otherwise = [x1] ++ unescape (x2:xs)

data Options = Options {
    searchPath :: Maybe [FilePath]
  , static :: Bool
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
    searchPath = Nothing
  , static = False
  }

pkgConfigWithOptions :: Options -> String -> IO (BuildFlags -> BuildFlags)
pkgConfigWithOptions options pkg = do
  resetPkgConfigPath <- case searchPath options of
                          Nothing -> return (return ())
                          Just path -> do
                            old <- lookupEnv "PKG_CONFIG_PATH"
                            setEnv "PKG_CONFIG_PATH" (intercalate [searchPathSeparator] path)
                            return $ case old of
                                      Nothing -> unsetEnv "PKG_CONFIG_PATH"
                                      Just value -> setEnv "PKG_CONFIG_PATH" value
  let flags = if static options then ["--static"] else []
  (code1, cflags, _) <- readProcessWithExitCode "pkg-config" (flags ++ ["--cflags", pkg]) ""
  (code2, ldflags, _) <- readProcessWithExitCode "pkg-config" (flags ++ ["--libs", pkg]) ""
  let result = if code1 == ExitSuccess && code2 == ExitSuccess
               then   parseCflags (parseFlags cflags)
                    . parseLibs (parseFlags ldflags)
               else const $ error $ "pkg-config: package " ++ pkg ++ " not found"
  resetPkgConfigPath
  return result

pkgConfig :: String -> IO (BuildFlags -> BuildFlags)
pkgConfig = pkgConfigWithOptions defaultOptions
