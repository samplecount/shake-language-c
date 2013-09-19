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

import Control.Applicative ((<$>))
import Data.List (isSuffixOf)
import Shakefile.C (BuildFlags, compilerFlags, linkerFlags)
import Shakefile.Lens (append)
import System.Process (readProcess)

-- ====================================================================
-- PkgConfig

pkgConfig :: String -> IO (BuildFlags -> BuildFlags)
pkgConfig pkg = do
    cflags <- parseFlags <$> readProcess "pkg-config" ["--cflags", pkg] ""
    lflags <- parseFlags <$> readProcess "pkg-config" ["--libs", pkg] ""
    return $ append compilerFlags [(Nothing, cflags)] . append linkerFlags lflags
    where
        parseFlags = map (dropSuffix "\\") . words . head . lines
        dropSuffix s x = if s `isSuffixOf` x
                         then reverse (drop (length s) (reverse x))
                         else x

