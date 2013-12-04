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

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Shakefile.C (
    under
  , mapFlag
  , concatMapFlag
  , (?=>)
  , onlyIf
  , notIf
  , Env
  , defaultEnv
  , buildPrefix
  , defaultBuildPrefix
  , Platform(..)
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
  , Language(..)
  , Linkage(..)
  , LinkResult(..)
  , BuildFlags
  , systemIncludes
  , userIncludes
  , defines
  , preprocessorFlags
  , compilerFlags
  , libraryPath
  , libraries
  , linkerFlags
  , staticLibraries
  , archiverFlags
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
  , Archiver
  , defaultArchiver
  , Linker
  , defaultLinker
  , executable
  , staticLibrary
  , sharedLibrary
  , dynamicLibrary
) where

import           Control.Applicative ((<$>))
import           Control.Lens hiding (Action, (<.>), under)
import           Control.Monad
import           Data.Char (toLower)
import           Development.Shake ((?>), need, readFile', system', systemOutput, want, writeFile')
import qualified Development.Shake as Shake
import           Development.Shake.FilePath
import           Data.Maybe
import           Data.Version
import           Shakefile.Lens (append)
import           Shakefile.SourceTree (SourceTree)
import qualified Shakefile.SourceTree as SourceTree
import           System.Environment (getEnvironment)

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
(?=>) :: FilePath -> (FilePath -> Shake.Action ()) -> Shake.Rules ()
f ?=> a = (equalFilePath f) ?> a

-- Function utils
onlyIf :: Bool -> (a -> a) -> (a -> a)
onlyIf b f = if b then f else id

notIf :: Bool -> (a -> a) -> (a -> a)
notIf b f = if b then id else f

data Env = Env {
    _buildPrefix :: FilePath
  } deriving (Show)

makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env "."

defaultBuildPrefix :: Target -> String -> FilePath
defaultBuildPrefix target config =
      "build"
  </> map toLower config
  </> (platformString $ _targetPlatform target)
  </> (archString $ _targetArch target)

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

makeLenses ''Target

mkTarget :: Arch -> String -> String -> Platform -> Target
mkTarget = Target

targetString :: Target -> String
targetString target =
     archShortString (target ^. targetArch)
  ++ "-" ++ (target ^. targetVendor)
  ++ "-" ++ (target ^. targetOS)

isTargetOS :: Maybe String -> Maybe String -> Target -> Bool
isTargetOS vendor os target =
    maybe True (_targetVendor target ==) vendor
 && maybe True (_targetOS target ==) os

data Language = C | Cpp | ObjC | ObjCpp
                 deriving (Enum, Eq, Show)

data Linkage = Static | Shared deriving (Enum, Eq, Show)

data LinkResult = Executable
                | SharedLibrary
                | DynamicLibrary
                deriving (Enum, Eq, Show)

defaultLanguageMap :: [(String, Language)]
defaultLanguageMap = concatMap f [
    (C, [".c"])
  , (Cpp, [".cc", ".CC", ".cpp", ".CPP", ".C", ".cxx", ".CXX"])
  , (ObjC, [".m"])
  , (ObjCpp, [".mm", ".M"])
  ]
  where f (lang, exts) = map (\ext -> (ext, lang)) exts

languageOf :: FilePath -> Maybe Language
languageOf = flip lookup defaultLanguageMap . takeExtension

data BuildFlags = BuildFlags {
    _systemIncludes :: [FilePath]
  , _userIncludes :: [FilePath]
  , _defines :: [(String, Maybe String)]
  , _preprocessorFlags :: [String]
  , _compilerFlags :: [(Maybe Language, [String])]
  , _libraryPath :: [FilePath]
  , _libraries :: [String]
  , _linkerFlags :: [String]
  -- This is needed for linking against local libraries built by shake (the linker `needs' its inputs).
  -- A better name maybe?
  , _staticLibraries :: [FilePath]
  , _archiverFlags :: [String]
  } deriving (Show)

makeLenses ''BuildFlags

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
  , _archiveFileName :: String -> FilePath
  , _linkerCmd :: String
  , _linker :: LinkResult -> Linker
  -- Not sure whether this should be someplace else
  , _linkResultFileName :: LinkResult -> String -> FilePath
  , _defaultBuildFlags :: BuildFlags -> BuildFlags
  }

makeLenses ''ToolChain

defaultArchiver :: Archiver
defaultArchiver toolChain buildFlags inputs output = do
    need inputs
    system' (tool archiverCmd toolChain)
        $ buildFlags ^. archiverFlags
        ++ [output]
        ++ inputs

defaultLinker :: Linker
defaultLinker toolChain buildFlags inputs output = do
    let staticLibs = buildFlags ^. staticLibraries
    need $ inputs ++ staticLibs
    system' (tool linkerCmd toolChain)
          $  inputs
          ++ buildFlags ^. linkerFlags
          ++ staticLibs
          ++ concatMapFlag "-L" (buildFlags ^. libraryPath)
          ++ concatMapFlag "-l" (buildFlags ^. libraries)
          ++ ["-o", output]

defaultToolChain :: ToolChain
defaultToolChain =
    ToolChain {
        _variant = GCC
      , _prefix = Nothing
      , _compilerCmd = "gcc"
      , _archiverCmd = "ar"
      , _archiver = defaultArchiver
      , _archiveFileName = ("lib"++) . (<.> "a")
      , _linkerCmd = "gcc"
      , _linker = \link toolChain ->
            case link of
                Executable -> defaultLinker toolChain
                _          -> defaultLinker toolChain . append linkerFlags ["-shared"]
      , _linkResultFileName = \linkResult ->
          case linkResult of
            Executable     -> id
            SharedLibrary  -> ("lib"++) . (<.> "so")
            DynamicLibrary ->             (<.> "so")
      , _defaultBuildFlags = id
      }

-- | Get the full path of an arbitrary toolchain command.
command :: String -> ToolChain -> FilePath
command cmd toolChain = maybe cmd (flip combine ("bin" </> cmd))
                                  (toolChain ^. prefix)

-- | Get the full path of a predefined tool.
tool :: (Getter ToolChain String) -> ToolChain -> FilePath
tool getter toolChain = command (toolChain ^. getter) toolChain

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

mkDefaultBuildFlags :: BuildFlags
mkDefaultBuildFlags =
    BuildFlags {
        _systemIncludes = []
      , _userIncludes = []
      , _defines = []
      , _preprocessorFlags = []
      , _compilerFlags = []
      , _libraryPath = []
      , _libraries = []
      , _linkerFlags = []
      , _staticLibraries = []
      , _archiverFlags = []
      }

defineFlags :: BuildFlags -> [String]
defineFlags = concatMapFlag "-D" . map (\(a, b) -> maybe a (\b' -> a++"="++b') b) . flip (^.) defines

compilerFlagsFor :: Maybe Language -> BuildFlags -> [String]
compilerFlagsFor lang = concat
                      . maybe (map snd . filter (isNothing.fst))
                              (mapMaybe . f) lang
                      . flip (^.) compilerFlags
    where f _ (Nothing, x) = Just x
          f l (Just l', x) | l == l' = Just x
                           | otherwise = Nothing

sed :: String -> FilePath -> FilePath -> Shake.Action ()
sed command input output = do
    need [input]
    (stdout, _) <- systemOutput "sed" ["-e", command, input]
    writeFile' output stdout

sourceTransform :: (FilePath -> FilePath) -> String -> FilePath -> Shake.Rules FilePath
sourceTransform f cmd input = do
    let output = f input
    output ?=> sed cmd input
    want [output]
    return output

dependencyFile :: ToolChain -> BuildFlags -> FilePath -> [FilePath] -> FilePath -> Shake.Rules ()
dependencyFile toolChain buildFlags input deps output = do
    output ?=> \_ -> do
        need $ [input] ++ deps
        system' (tool compilerCmd toolChain)
                $  concatMapFlag "-I" (buildFlags ^. systemIncludes)
                ++ mapFlag "-iquote" (buildFlags ^. userIncludes)
                ++ (defineFlags buildFlags)
                ++ (buildFlags ^. preprocessorFlags)
                ++ (compilerFlagsFor (languageOf input) buildFlags)
                ++ ["-MM", "-o", output, input]

parseDependencies :: String -> [FilePath]
parseDependencies = drop 2 . words . filter (/= '\\')

type ObjectRule = ToolChain -> BuildFlags -> FilePath -> [FilePath] -> FilePath -> Shake.Rules ()

staticObject :: ObjectRule
staticObject toolChain buildFlags input deps output = do
    let depFile = output <.> "d"
    dependencyFile toolChain buildFlags input deps depFile
    output ?=> \_ -> do
        deps' <- parseDependencies <$> readFile' depFile
        need $ [input] ++ deps ++ deps'
        system' (tool compilerCmd toolChain)
                $  concatMapFlag "-I" (buildFlags ^. systemIncludes)
                ++ mapFlag "-iquote" (buildFlags ^. userIncludes)
                ++ (defineFlags buildFlags)
                ++ (buildFlags ^. preprocessorFlags)
                ++ (compilerFlagsFor (languageOf input) buildFlags)
                ++ ["-c", "-o", output, input]

sharedObject :: ObjectRule
sharedObject toolChain = staticObject toolChain -- Disable for now: . append compilerFlags [(Nothing, ["-fPIC"])]

mkObjectsDir :: Env -> Target -> FilePath -> FilePath
mkObjectsDir env target path = (env ^. buildPrefix) </> map tr (makeRelative "/" path) ++ "_obj"
    where tr '.' = '_'
          tr x   = x

mkBuildPath :: Env -> Target -> FilePath -> FilePath
mkBuildPath env target path = (env ^. buildPrefix) </> makeRelative "/" path

buildProduct :: ObjectRule -> Linker -> FilePath
             -> Env -> Target -> ToolChain
             -> SourceTree BuildFlags
             -> Shake.Rules FilePath
buildProduct object link fileName env target toolChain sources = do
    let resultPath = mkBuildPath env target fileName
        objectsDir = mkObjectsDir env target fileName
        sources' = SourceTree.flags (toolChain ^. defaultBuildFlags) sources
    objects <- forM (SourceTree.apply mkDefaultBuildFlags sources') $ \(buildFlags, (src, deps)) -> do
        let obj = objectsDir </> makeRelative "/" (src <.> "o")
        object toolChain buildFlags src deps obj
        return obj
    resultPath ?=> link toolChain (SourceTree.collect mkDefaultBuildFlags sources') objects
    return resultPath

-- | Rule for building an executable.
executable :: Env -> Target -> ToolChain -> String -> SourceTree BuildFlags -> Shake.Rules FilePath
executable env target toolChain name sources =
    buildProduct
        staticObject
        ((toolChain ^. linker) Executable)
        ((toolChain ^. linkResultFileName) Executable name)
        env target toolChain sources

-- | Rule for building a static library.
staticLibrary :: Env -> Target -> ToolChain -> String -> SourceTree BuildFlags -> Shake.Rules FilePath
staticLibrary env target toolChain name sources =
    buildProduct
        staticObject
        (toolChain ^. archiver)
        ((toolChain ^. archiveFileName) name)
        env target toolChain sources

-- | Rule for building a shared library.
sharedLibrary :: Env -> Target -> ToolChain -> String -> SourceTree BuildFlags -> Shake.Rules FilePath
sharedLibrary env target toolChain name sources =
    buildProduct
        sharedObject
        ((toolChain ^. linker) SharedLibrary)
        ((toolChain ^. linkResultFileName) SharedLibrary name)
        env target toolChain sources

-- | Rule for building a dynamic library.
dynamicLibrary :: Env -> Target -> ToolChain -> String -> SourceTree BuildFlags -> Shake.Rules FilePath
dynamicLibrary env target toolChain name sources =
    buildProduct
        sharedObject
        ((toolChain ^. linker) DynamicLibrary)
        ((toolChain ^. linkResultFileName) DynamicLibrary name)
        env target toolChain sources
