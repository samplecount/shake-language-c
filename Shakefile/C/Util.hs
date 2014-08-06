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

module Shakefile.C.Util (
    under
  , mapFlag
  , concatMapFlag
  , (?=>)
  , onlyIf
  , notIf
  , words'
) where

import Data.List
import Development.Shake
import Development.Shake.FilePath

{-import Debug.Trace-}

under :: FilePath -> [FilePath] -> [FilePath]
under dir = map prependDir
    where prependDir ""   = dir
          prependDir "."  = dir
          prependDir ".." = takeDirectory dir
          prependDir x    = combine dir x

mapFlag :: String -> [String] -> [String]
mapFlag f = concatMap (\x -> [f, x])

concatMapFlag :: String -> [String] -> [String]
concatMapFlag f = map (f++)

-- Shake utils
(?=>) :: FilePath -> (FilePath -> Action ()) -> Rules ()
f ?=> a = (equalFilePath f) ?> a

-- Function utils
onlyIf :: Bool -> (a -> a) -> (a -> a)
onlyIf b f = if b then f else id

notIf :: Bool -> (a -> a) -> (a -> a)
notIf b f = if b then id else f

-- | Splits a list of space separated strings.
--
-- Spaces can be escaped by '\\'.
words' :: String -> [String]
words' = unescape . words
  where
    escape = "\\"
    escapeLength = length escape
    isEscaped = isSuffixOf escape
    dropEscape = (++" ") . reverse . drop escapeLength . reverse
    unescape [] = []
    unescape [x] = [if isEscaped x then dropEscape x else x]
    unescape (x1:x2:xs)
      | isEscaped x1 = unescape ((dropEscape x1 ++ x2):xs)
      | otherwise = [x1] ++ unescape (x2:xs)
