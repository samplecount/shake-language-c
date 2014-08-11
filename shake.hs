#!/usr/bin/env runhaskell -I0

-- Copyright 2013-2014 Samplecount S.L.
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

import           Control.Applicative
import qualified Data.List as List
import           GHC.Conc (getNumProcessors)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           System.FilePath
import qualified System.Info as System
import qualified System.Process as Proc
import           Text.Read (readEither)

ghc = System.compilerName
sandbox = ".cabal-sandbox"
shake = sandbox </> "shake"
cabal cmd args = Proc.callProcess "cabal" (cmd:args)

getPackageDatabase = do
  let os = case System.os of
            "darwin" -> "osx"
            x        -> x
  v <- (head.lines) <$> Proc.readProcess ghc ["--numeric-version"] ""
  return $ List.intercalate "-" [System.arch, os, ghc, v, "packages.conf.d"]

readConfigFile :: FilePath -> IO [(String,String)]
readConfigFile path = do
  b <- Dir.doesFileExist path
  case b of
    True -> do
      contents <- readFile path
      case readEither contents of
        Left e -> error $ path ++ ": " ++ e
        Right c -> return c
    False -> return []

update cfg = do
  cabal "sandbox" ["init"]
  case lookup "source-packages" cfg of
    Nothing -> return ()
    Just xs -> cabal "sandbox" ("add-source":words xs)
  case lookup "dependencies" cfg of
    Nothing -> return ()
    Just xs -> do
      jobs <- (("-j"++) . show) <$> getNumProcessors
      cabal "install" (jobs:words xs)

build cfg args = do
  Dir.createDirectoryIfMissing True shake

  package_db <- combine sandbox <$> getPackageDatabase

  jobs <- (("-j"++) . show) <$> getNumProcessors

  Proc.callProcess ghc $
    [ "-package-db", package_db
    , "--make", maybe "shakefile.hs" id (lookup "script" cfg)
    , jobs ]
    ++ map ("-i"++)
           (maybe [] words (lookup "source-directories" cfg))
    ++ [ "-rtsopts", "-with-rtsopts=-I0"
       , "-outputdir=" ++ shake
       , "-o", shake </> "build" ]

  Proc.callProcess (shake </> "build") (jobs:args)

main :: IO ()
main = do
  args <- Env.getArgs
  cfg <- readConfigFile "shake.cfg"
  case args of
    [".update"] -> do
      update cfg
      build cfg []
    args -> do
      b1 <- Dir.doesDirectoryExist sandbox
      b2 <- Dir.doesFileExist (shake </> "build")
      if b1 && b2
        then return ()
        else update cfg
      build cfg args
