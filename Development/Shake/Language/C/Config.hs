{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Development.Shake.Language.C.Config(
    withConfig
  , getList
) where

import Control.Applicative
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config (readConfigFile)
import Development.Shake.Language.C.Util (words')

newtype Config = Config (FilePath, String) deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

withConfig :: [FilePath] -> Rules (FilePath -> String -> Action (Maybe String))
withConfig deps = do
  fileCache <- newCache $ \file -> do
    need deps
    liftIO $ readConfigFile file
  query <- addOracle $ \(Config (file, key)) -> Map.lookup key <$> fileCache file
  return $ \file key -> query (Config (file, key))

getList :: (String -> Action (Maybe String)) -> [String] -> Action [String]
getList getConfig keys = do
    sources <- mapM (fmap (fmap words') . getConfig) keys
    return $ concatMap (maybe [] id) sources
