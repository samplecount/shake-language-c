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
    pkgConfig
) where

import Data.List (isSuffixOf)
import Shakefile.C (BuildFlags, compilerFlags, linkerFlags)
import Shakefile.Lens (append)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-- ====================================================================
-- PkgConfig

pkgConfig :: String -> IO (BuildFlags -> BuildFlags)
pkgConfig pkg = do
    (code1, cflags, _) <- readProcessWithExitCode "pkg-config" ["--cflags", pkg] ""
    case code1 of
        ExitSuccess -> do
            (code2, ldflags, _) <- readProcessWithExitCode "pkg-config" ["--libs", pkg] ""
            case code2 of
                ExitSuccess ->
                    return $ append compilerFlags [(Nothing, parseFlags cflags)]
                           . append linkerFlags (parseFlags ldflags)
                _ -> notFound
        _ -> notFound
    where
        notFound = do
            putStrLn $ "WARNING: package " ++ pkg ++ " not found (pkgconfig)"
            return id
        parseFlags = map (dropSuffix "\\") . words . head . lines
        dropSuffix s x = if s `isSuffixOf` x
                         then reverse (drop (length s) (reverse x))
                         else x
