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
{-# LANGUAGE TypeOperators #-}

module Development.Shake.Language.C.ToolChain (
    Linkage(..)
  , LinkResult(..)
  , ToolChain
  , ToolChainVariant(..)
  , defaultToolChain
  , toolDirectory
  , toolPrefix
  , variant
  , compilerCommand
  , compiler
  , archiverCommand
  , archiver
  , linkerCommand
  , linker
  , defaultBuildFlags
  , defaultCompiler
  , Archiver
  , defaultArchiver
  , Linker
  , defaultLinker
  , toolFromString
  , tool
  , applyEnv
  , toEnv
) where

import           Control.Applicative
import           Data.Char (toLower)
import           Data.List (isInfixOf, isSuffixOf)
import           Data.Monoid (mempty)
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util (needMakefileDependencies)
import           Development.Shake.Language.C.Label
import           Development.Shake.Language.C.BuildFlags
import           Development.Shake.Language.C.Language (Language(..), languageOf)
import           Development.Shake.Language.C.Util

data Linkage = Static | Shared deriving (Enum, Eq, Show)

data LinkResult = Executable
                | SharedLibrary
                | LoadableLibrary
                deriving (Enum, Eq, Show)

data ToolChainVariant =
    Generic
  | GCC
  | LLVM
  deriving (Eq, Show)

type Compiler = ToolChain -> BuildFlags -> FilePath -> FilePath -> Action ()
type Linker   = ToolChain -> BuildFlags -> [FilePath] -> FilePath -> Action ()
type Archiver = ToolChain -> BuildFlags -> [FilePath] -> FilePath -> Action ()

data ToolChain = ToolChain {
    _variant :: ToolChainVariant
  , _toolDirectory :: Maybe FilePath
  , _toolPrefix :: String
  , _compilerCommand :: FilePath
  , _compiler :: Compiler
  , _archiverCommand :: FilePath
  , _archiver :: Archiver
  , _linkerCommand :: FilePath
  , _linker :: LinkResult -> Linker
  , _defaultBuildFlags :: Action (BuildFlags -> BuildFlags)
  }

mkLabel ''ToolChain

defaultCompiler :: Compiler
defaultCompiler toolChain buildFlags input output = do
  need $ [input]
  let depFile = output <.> "d"
  command_ [] (tool toolChain compilerCommand)
    $  concatMapFlag "-I" (get systemIncludes buildFlags)
    ++ mapFlag "-iquote" (get userIncludes buildFlags)
    ++ defineFlags buildFlags
    ++ get preprocessorFlags buildFlags
    ++ compilerFlagsFor (languageOf input) buildFlags
    ++ ["-MD", "-MF", depFile]
    ++ ["-c", "-o", output, input]
  needMakefileDependencies depFile

defaultArchiver :: Archiver
defaultArchiver toolChain buildFlags inputs output = do
    need inputs
    command_ [] (tool toolChain archiverCommand)
        $ get archiverFlags buildFlags
        ++ [output]
        ++ inputs

defaultLinker :: Linker
defaultLinker toolChain buildFlags inputs output = do
    let localLibs = get localLibraries buildFlags
        buildFlags' = append libraryPath (map takeDirectory localLibs)
                      -- Local libraries must be passed to the linker before system libraries they depend on
                    . prepend libraries (map (strip.dropExtension.takeFileName) localLibs)
                    $ buildFlags
    need $ inputs ++ localLibs
    command_ [] (tool toolChain linkerCommand)
          $  inputs
          ++ get linkerFlags buildFlags'
          ++ concatMapFlag "-L" (get libraryPath buildFlags')
          ++ concatMapFlag "-l" (get libraries buildFlags')
          ++ ["-o", output]
    where
      strip ('l':'i':'b':rest) = rest
      strip x = x

defaultToolChain :: ToolChain
defaultToolChain =
    ToolChain {
        _variant = GCC
      , _toolDirectory = Nothing
      , _toolPrefix = ""
      , _compilerCommand = "gcc"
      , _compiler = defaultCompiler
      , _archiverCommand = "ar"
      , _archiver = defaultArchiver
      , _linkerCommand = "gcc"
      , _linker = \linkResult linkerCmd ->
          case linkResult of
            Executable -> defaultLinker linkerCmd
            _          -> defaultLinker linkerCmd . append linkerFlags ["-shared"]
      , _defaultBuildFlags = return id
      }

toolFromString :: ToolChain -> String -> FilePath
toolFromString toolChain name =
  let c = _toolPrefix toolChain ++ name
  in maybe c (</> c) (_toolDirectory toolChain)

-- | Get the full path of a predefined tool.
tool :: ToolChain -> (ToolChain :-> String) -> FilePath
tool toolChain getter = toolFromString toolChain (get getter toolChain)

applyEnv :: ToolChain -> Action ToolChain
applyEnv toolChain = do
  cc <- getEnv "CC"
  vendor <- getEnv "STIR_TOOLCHAIN_VENDOR"
  return $ maybe id (set compilerCommand) cc
         . maybe id (set variant) ((vendor >>= parseVendor) <|> (cc >>= vendorFromCommand))
         $ toolChain
  where
    parseVendor s =
      case map toLower s of
        "gcc" -> Just GCC
        "llvm" -> Just LLVM
        "clang" -> Just LLVM
        _ -> Nothing
    vendorFromCommand path =
      let x = takeFileName path
      in if "gcc" `isInfixOf` x || "g++" `isInfixOf` x
         then Just GCC
         else if "clang" `isInfixOf` x
         then Just LLVM
         else Just Generic

-- | Export a 'ToolChain' definition to a list of environment variable mappings, suitable e.g. for calling third-party configure scripts.
--
-- Needs some fleshing out; currently only works for "standard" binutil toolchains.
toEnv :: ToolChain -> Action [(String,String)]
toEnv tc = do
  flags <- (\f -> f mempty) <$> get defaultBuildFlags tc
  let cflags =  concatMapFlag "-I" (map escapeSpaces (get systemIncludes flags))
             ++ concatMapFlag "-I" (map escapeSpaces (get userIncludes flags))
             ++ defineFlags flags
             ++ get preprocessorFlags flags
             ++ compilerFlagsFor (Just C) flags
      cxxflags = cflags ++ compilerFlagsFor (Just Cpp) flags
      ldflags =  get linkerFlags flags
              ++ concatMapFlag "-L" (get libraryPath flags)
              ++ concatMapFlag "-l" (get libraries flags)
      c2cxx path = let x = takeFileName path
                   in if "gcc" `isSuffixOf` x
                      then (++"g++") . reverse . drop 3 . reverse $ path
                      else if "clang" `isSuffixOf` x
                           then path ++ "++"
                           else path
  let cc = tool tc compilerCommand
      cpp = toolFromString tc "cpp"
  cppExists <- doesFileExist cpp
  let cpp' = if cppExists then cpp else cc ++ " -E"
  return $ [
      ("CPP", cpp')
    , ("AR", tool tc archiverCommand)
    , ("NM", toolFromString tc "nm")
    , ("CC", tool tc compilerCommand)
    -- FIXME: Configure toolchain with compiler command per language?
    , ("CXX", c2cxx $ tool tc compilerCommand)
    , ("LD", toolFromString tc "ld")
    , ("RANLIB", toolFromString tc "ranlib")
    , ("CFLAGS", unwords cflags)
    , ("CPPFLAGS", unwords cflags)
    , ("CXXFLAGS", unwords cxxflags)
    , ("LDFLAGS", unwords ldflags)
    ]
