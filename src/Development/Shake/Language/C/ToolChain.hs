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

{-|
Description: Types and functions for working with target tool chains
-}

module Development.Shake.Language.C.ToolChain (
    Linkage(..)
    -- ** Working with toolchains
  , ToolChain
  , ToolChainVariant(..)
  , toolDirectory       -- | Directory prefix for tools in a `ToolChain`, e.g. @\/usr\/local\/linux-armv5-eabi\/bin@.
  , toolPrefix          -- | Prefix string for tools in a `ToolChain`, e.g. @"linux-armv5-eabi-"@.
  , variant             -- | Toolchain variant.
  , compilerCommand     -- | Compiler command, usually used in the `compiler` action.
  , Compiler
  , compiler            -- | Compiler action for this `ToolChain`.
  , archiverCommand     -- | Archiver command, usually used in the `archiver` action.
  , Archiver
  , archiver            -- | Archiver action for this `ToolChain`.
  , linkerCommand       -- | Linker command, usually used in the `linker` action.
  , Linker
  , LinkResult(..)
  , linker              -- | Linker action for this `ToolChain`.
  , defaultBuildFlags   -- | Action returning the default `BuildFlags` for this `ToolChain`.

  -- ** Interfacing with other build systems
  , applyEnv
  , toEnv
  -- ** Utilities for toolchain writers
  , defaultToolChain
  , defaultCompiler
  , defaultArchiver
  , defaultLinker
  , toolFromString
  , tool
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
import           Development.Shake.Language.C.Language (languageOf)
import           Development.Shake.Language.C.Util

-- | Linkage type, static or shared.
data Linkage = Static | Shared deriving (Enum, Eq, Show)

-- | Link result type
data LinkResult =
    Executable      -- ^ Executable
  | SharedLibrary   -- ^ Shared (dynamically linked) library
  | LoadableLibrary -- ^ Dynamically loadable library
  deriving (Enum, Eq, Show)

-- | Toolchain variant.
data ToolChainVariant =
    Generic   -- ^ Unspecified toolchain
  | GCC       -- ^ GNU Compiler Collection (gcc) toolchain
  | LLVM      -- ^ Low-Level Virtual Machine (LLVM) toolchain
  deriving (Eq, Show)

-- | `Action` type for producing an object file from a source file.
type Compiler =
     ToolChain  -- ^ Toolchain
  -> BuildFlags -- ^ Compiler flags
  -> FilePath   -- ^ Input source file
  -> FilePath   -- ^ Output object file
  -> Action ()

-- | `Action` type for linking object files into an executable or a library.
type Linker   =
     ToolChain  -- ^ Toolchain
  -> BuildFlags -- ^ Linker flags
  -> [FilePath] -- ^ Input object files
  -> FilePath   -- ^ Output link product
  -> Action ()

-- | `Action` type for archiving object files into a static library.
type Archiver =
    ToolChain   -- ^ Toolchain
 -> BuildFlags  -- ^ Archiver flags
 -> [FilePath]  -- ^ Input object files
 -> FilePath    -- ^ Output object archive (static library)
 -> Action ()

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

-- | Default compiler action.
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

-- | Default archiver action.
defaultArchiver :: Archiver
defaultArchiver toolChain buildFlags inputs output = do
    need inputs
    command_ [] (tool toolChain archiverCommand)
        $ get archiverFlags buildFlags
        ++ [output]
        ++ inputs

-- | Default linker action.
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

-- | Default toolchain.
--
-- Probably not useful without modification.
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

-- | Given a tool chain command name, construct the command's full path, taking into account the toolchain's `toolPrefix`.
toolFromString ::
    ToolChain -- ^ Toolchain
 -> String    -- ^ Command name
 -> FilePath  -- ^ Full command path
toolFromString toolChain name =
  let c = _toolPrefix toolChain ++ name
  in maybe c (</> c) (_toolDirectory toolChain)

-- | Construct the full path of a predefined tool given a `ToolChain` accessor.
tool ::
    ToolChain               -- ^ Toolchain
 -> (ToolChain :-> String)  -- ^ Toolchain accessor
 -> FilePath                -- ^ Full command path
tool toolChain getter = toolFromString toolChain (get getter toolChain)

-- | Apply the current environment and return a modified toolchain.
--
-- This function is experimental and subject to change!
--
-- Currently recognised environment variables are
--
--  [@CC@] Path to @C@ compiler.
--
--  [@SHAKE_TOOLCHAIN_VARIANT@] One of the values of 'ToolChainVariant' (case insensitive). If this variable is not present, an attempt is made to determine the toolchain variant from the @C@ compiler command.
applyEnv :: ToolChain -> Action ToolChain
applyEnv toolChain = do
  cc <- getEnv "CC"
  vendor <- getEnv "SHAKE_TOOLCHAIN_VARIANT"
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

-- | Export a 'ToolChain' definition to a list of environment variable mappings, suitable e.g. for calling third-party configure scripts in cross-compilation mode.
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
