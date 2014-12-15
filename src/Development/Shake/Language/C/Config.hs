{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

{-|
Description: Read values from configuration files

This module provides utilities for reading values from configuration files,
similar to the functions provided by "Development.Shake.Config".
-}
module Development.Shake.Language.C.Config(
    withConfig
  , parsePaths
  , getPaths
) where

import Control.Applicative
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config (readConfigFileWithEnv)
import Development.Shake.Language.C.Util (words')

newtype Config = Config ([(String, String)], FilePath, String) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

{- | Given a list of dependencies, return a function that takes an environment
of variable bindings, a configuration file path and a configuration variable
and returns the corresponding configuration value.

This function is more flexible than `Development.Shake.Config.usingConfigFile`.
It allows the use of multiple configuration files as well as specifying default
variable bindings.

Typical usage would be something like this:

> -- In the Rules monad
> getConfig <- withConfig []
> -- Then in an Action
> ... value <- getConfig [("variable", "default value")] "config.cfg" "variable"
-}
withConfig :: [FilePath]
           -> Rules (   [(String,String)]
                     -> FilePath
                     -> String
                     -> Action (Maybe String))
withConfig deps = do
  fileCache <- newCache $ \(env, file) -> do
    need deps
    liftIO $ readConfigFileWithEnv env file
  query <- addOracle $ \(Config (env, file, key)) -> Map.lookup key <$> fileCache (env, file)
  return $ \env file key -> query (Config (env, file, key))

-- | Parse a list of space separated paths from an input string. Spaces can be escaped by @\\@ characters.
--
-- >>> parsePaths "/a /a/b /a/b/c\\ d"
-- ["/a","/a/b","/a/b/c d"]
parsePaths :: String -> [FilePath]
parsePaths = words'

-- | Given a function that maps a configuration variable to a value and a list of variable names, return a corresponding list of file paths. Missing variables are ignored.
getPaths ::
    (String -> Action (Maybe String)) -- ^ Configuration lookup function
 -> [String]                          -- ^ Configuration keys
 -> Action [FilePath]                 -- ^ File paths
getPaths getConfig keys = do
    sources <- mapM (fmap (fmap parsePaths) . getConfig) keys
    return $ concatMap (maybe [] id) sources
