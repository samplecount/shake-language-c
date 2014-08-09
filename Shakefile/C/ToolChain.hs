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

module Shakefile.C.ToolChain (
    Platform(..)
  , platformString
  , Arch(..)
  , archShortString
  , archString
  , ArmVersion(..)
  , X86Version(..)
  , Target
  , mkTarget
  , targetArch
  , targetVendor
  , targetOS
  , targetPlatform
  , isTargetOS
  , Linkage(..)
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
  , toolChainFromEnvironment
  , applyEnv
  , ToBuildPrefix(..)
) where

import           Control.Applicative
import           Data.Char (toLower)
import           Data.List (isInfixOf)
import           Development.Shake hiding (command)
import qualified Development.Shake as Shake
import           Development.Shake.FilePath
import           Development.Shake.Util (needMakefileDependencies)
import           Data.Version
import           Shakefile.Label ((:->), append, get, mkLabel, prepend, set)
import           Shakefile.C.BuildFlags hiding (defaultBuildFlags)
import           Shakefile.C.Language (languageOf)
import           Shakefile.C.Util (concatMapFlag, mapFlag)
import           System.Environment (getEnvironment)

data Platform = Platform {
    platformName :: String
  , platformVersion :: Version
  } deriving (Eq, Show)

platformString :: Platform -> String
platformString = map toLower . platformName

data X86Version =
    I386
  | I686
  | X86_64
  deriving (Eq, Show)

data ArmVersion =
    Armv5
  | Armv6
  | Armv7
  | Armv7s
  deriving (Eq, Show)

data Arch =
    X86 X86Version
  | Arm ArmVersion
  | LLVM_IR
  deriving (Eq, Show)

archShortString :: Arch -> String
archShortString arch =
  case arch of
    X86 _ -> "x86"
    Arm _ -> "arm"
    LLVM_IR -> "llvm_ir"

archString :: Arch -> String
archString arch =
  case arch of
    X86 I386 -> "i386"
    X86 I686 -> "i686"
    X86 X86_64 -> "x86_64"
    Arm Armv5 -> "armv5"
    Arm Armv6 -> "armv6"
    Arm Armv7 -> "armv7"
    Arm Armv7s -> "armv7s"
    LLVM_IR -> "llvm_ir"

data Target = Target {
    _targetArch :: Arch
  , _targetVendor :: String
  , _targetOS :: String
  , _targetPlatform :: Platform
  } deriving (Show)

mkLabel ''Target

mkTarget :: Arch -> String -> String -> Platform -> Target
mkTarget = Target

isTargetOS :: Maybe String -> Maybe String -> Target -> Bool
isTargetOS vendor os target =
    maybe True (_targetVendor target ==) vendor
 && maybe True (_targetOS target ==) os

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
  , _defaultBuildFlags :: BuildFlags -> BuildFlags
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
      , _defaultBuildFlags = id
      }

toolFromString :: ToolChain -> String -> FilePath
toolFromString toolChain name =
  let cmd = _toolPrefix toolChain ++ name
  in maybe cmd (</> cmd) (_toolDirectory toolChain)

-- | Get the full path of a predefined tool.
tool :: ToolChain -> (ToolChain :-> String) -> FilePath
tool toolChain getter = toolFromString toolChain (get getter toolChain)

toolChainFromEnvironment :: Monad m => m (ToolChain -> ToolChain)
toolChainFromEnvironment = return id

applyEnv :: ToolChain -> Action ToolChain
applyEnv toolChain = do
  compiler <- getEnv "CC"
  vendor <- getEnv "STIR_TOOLCHAIN_VENDOR"
  return $ maybe id (set compilerCommand) compiler
         . maybe id (set variant) ((vendor >>= parseVendor) <|> (compiler >>= vendorFromCommand))
         $ toolChain
  where
    parseVendor s =
      case map toLower s of
        "gcc" -> Just GCC
        "llvm" -> Just LLVM
        "clang" -> Just LLVM
        _ -> Nothing
    vendorFromCommand path =
      let cmd = takeFileName path
      in if "gcc" `isInfixOf` cmd || "g++" `isInfixOf` cmd
         then Just GCC
         else if "clang" `isInfixOf` cmd
         then Just LLVM
         else Just Generic

class ToBuildPrefix a where
  toBuildPrefix :: a -> FilePath

instance ToBuildPrefix Platform where
  toBuildPrefix = platformString

instance ToBuildPrefix Target where
   toBuildPrefix target =
          toBuildPrefix (get targetPlatform target)
      </> (archString $ get targetArch target)
