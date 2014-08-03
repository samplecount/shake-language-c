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

module Shakefile.SourceTree (
    SourceTree
  , node
  , empty
  , flagsM
  , flags
  , filesWithDeps
  , files
  , list
  , append
  , flatten
  , collect
) where

import Control.Monad (liftM2)
import Data.Tree (Tree(Node))

-- | A tree with a transformation and a list of files and their dependencies at each node.
type SourceTree m a = Tree (m (a -> a), [(FilePath, [FilePath])])

compose :: Monad m => m (b -> c) -> m (a -> b) -> m (a -> c)
compose = liftM2 (.)

node :: m (a -> a) -> [(FilePath, [FilePath])] -> [SourceTree m a] -> SourceTree m a
node f fs = Node (f, fs)

empty :: Monad m => SourceTree m a
empty = node (return id) [] []

flagsM :: Monad m => m (a -> a) -> SourceTree m a -> SourceTree m a
flagsM f t = node f [] [t]

flags :: Monad m => (a -> a) -> SourceTree m a -> SourceTree m a
flags f = flagsM (return f)

filesWithDeps :: Monad m => [(FilePath, [FilePath])] -> SourceTree m a
filesWithDeps fs = node (return id) fs []

files :: Monad m => [FilePath] -> SourceTree m a
files = filesWithDeps . map (flip (,) [])

list :: Monad m => [SourceTree m a] -> SourceTree m a
list = node (return id) []

append :: SourceTree m a -> SourceTree m a -> SourceTree m a
append (Node x ts) t = Node x (ts ++ [t])

flatten :: Monad m => SourceTree m a -> [(m (a -> a), (FilePath, [FilePath]))]
flatten = go (return id)
    where
        distribute a = map ((,)a)
        go g (Node (f, fs) []) = distribute (compose f g) fs
        go g (Node (f, fs) ns) = let h = compose f g
                                 in distribute h fs ++ concatMap (go h) ns

collect :: Monad m => SourceTree m a -> m (a -> a)
collect t = go (return id) t
    where
        go g (Node (f, _) []) = compose f g
        go g (Node (f, _) ns) = foldl go (compose f g) ns
