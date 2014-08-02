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
  , targetString
  , isTargetOS
  , Linkage(..)
  , LinkResult(..)
  , ToolChain
  , ToolChainVariant(..)
  , defaultToolChain
  , toolChainFromEnvironment
  , variant
  , prefix
  , compilerCmd
  , archiverCmd
  , archiver
  , archiveFileName
  , linkerCmd
  , linker
  , linkResultFileName
  , defaultBuildFlags
  , command
  , tool
  , objectFile
  , Archiver
  , defaultArchiver
  , Linker
  , defaultLinker
  , ToBuildPrefix(..)
) where

import           Data.Char (toLower)
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

targetString :: Target -> String
targetString target =
     archShortString (get targetArch target)
  ++ "-" ++ get targetVendor target
  ++ "-" ++ get targetOS target

isTargetOS :: Maybe String -> Maybe String -> Target -> Bool
isTargetOS vendor os target =
    maybe True (_targetVendor target ==) vendor
 && maybe True (_targetOS target ==) os

data Linkage = Static | Shared deriving (Enum, Eq, Show)

data LinkResult = Executable
                | SharedLibrary
                | DynamicLibrary
                deriving (Enum, Eq, Show)

data ToolChainVariant =
    Generic
  | GCC
  | LLVM
  deriving (Eq, Show)

type Linker = ToolChain -> BuildFlags -> [FilePath] -> FilePath -> Shake.Action ()
type Archiver = Linker

data ToolChain = ToolChain {
    _variant :: ToolChainVariant
  , _prefix :: Maybe FilePath
  , _compilerCmd :: String
  , _archiverCmd :: String
  , _archiver :: Archiver
  , _archiveFileName :: FilePath -> FilePath
  , _linkerCmd :: String
  , _linker :: LinkResult -> Linker
  -- Not sure whether this should be someplace else
  , _linkResultFileName :: LinkResult -> FilePath -> FilePath
  , _defaultBuildFlags :: BuildFlags -> BuildFlags
  }

mkLabel ''ToolChain

defaultArchiver :: Archiver
defaultArchiver toolChain buildFlags inputs output = do
    need inputs
    command_ [] (tool archiverCmd toolChain)
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
    command_ [] (tool linkerCmd toolChain)
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
      , _prefix = Nothing
      , _compilerCmd = "gcc"
      , _archiverCmd = "ar"
      , _archiver = defaultArchiver
      , _archiveFileName = (<.> "a")
      , _linkerCmd = "gcc"
      , _linker = \link toolChain ->
            case link of
                Executable -> defaultLinker toolChain
                _          -> defaultLinker toolChain . append linkerFlags ["-shared"]
      , _linkResultFileName = \linkResult ->
          case linkResult of
            Executable     -> id
            SharedLibrary  -> (<.> "so")
            DynamicLibrary -> (<.> "so")
      , _defaultBuildFlags = id
      }

-- | Get the full path of an arbitrary toolchain command.
command :: String -> ToolChain -> FilePath
command name toolChain = maybe name (flip combine ("bin" </> name))
                                    (get prefix toolChain)

-- | Get the full path of a predefined tool.
tool :: (ToolChain :-> String) -> ToolChain -> FilePath
tool getter toolChain = command (get getter toolChain) toolChain

toolChainFromEnvironment :: IO (ToolChain -> ToolChain)
toolChainFromEnvironment = do
  env <- getEnvironment
  return $ apply env (set compilerCmd) "CC"
         . apply env (set variant . parseVariant) "TOOLCHAIN_VARIANT"
  where
    apply env f k = maybe id f (lookup k env)
    parseVariant s =
      case map toLower s of
        "gcc" -> GCC
        "llvm" -> LLVM
        "clang" -> LLVM
        _ -> Generic

objectFile :: ToolChain -> BuildFlags -> FilePath -> [FilePath] -> FilePath -> Action ()
objectFile toolChain buildFlags input deps output = do
  need $ [input] ++ deps
  let depFile = output <.> "d"
  command_ [] (tool compilerCmd toolChain)
    $  concatMapFlag "-I" (get systemIncludes buildFlags)
    ++ mapFlag "-iquote" (get userIncludes buildFlags)
    ++ defineFlags buildFlags
    ++ get preprocessorFlags buildFlags
    ++ compilerFlagsFor (languageOf input) buildFlags
    ++ ["-MD", "-MF", depFile]
    ++ ["-c", "-o", output, input]
  needMakefileDependencies depFile

class ToBuildPrefix a where
  toBuildPrefix :: a -> FilePath

instance ToBuildPrefix Platform where
  toBuildPrefix = platformString

instance ToBuildPrefix Target where
   toBuildPrefix target =
          toBuildPrefix (get targetPlatform target)
      </> (archString $ get targetArch target)
