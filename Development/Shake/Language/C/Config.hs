{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

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

withConfig :: [FilePath] -> Rules ([(String,String)] -> FilePath -> String -> Action (Maybe String))
withConfig deps = do
  fileCache <- newCache $ \(env, file) -> do
    need deps
    liftIO $ readConfigFileWithEnv env file
  query <- addOracle $ \(Config (env, file, key)) -> Map.lookup key <$> fileCache (env, file)
  return $ \env file key -> query (Config (env, file, key))

parsePaths :: String -> [FilePath]
parsePaths = words'

getPaths :: (String -> Action (Maybe String)) -> [String] -> Action [String]
getPaths getConfig keys = do
    sources <- mapM (fmap (fmap parsePaths) . getConfig) keys
    return $ concatMap (maybe [] id) sources
