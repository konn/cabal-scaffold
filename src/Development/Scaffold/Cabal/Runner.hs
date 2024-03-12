{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Development.Scaffold.Cabal.Runner (runApp, App (..)) where

import Data.Generics.Labels ()
import qualified Data.Yaml as Y
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants
import Path
import Path.IO (doesFileExist, getCurrentDir, makeAbsolute)
import RIO
import RIO.Orphans (HasResourceMap (..), ResourceMap, withResourceMap)

findYamlConfigPathIn :: (MonadUnliftIO m) => Path b Dir -> m (Maybe (Path Abs File))
findYamlConfigPathIn dir = do
  workDir <- makeAbsolute dir
  flip fix workDir \self cwd -> do
    let configPath = cwd </> [relfile|cabal-scaffold.yaml|]
    there <- doesFileExist configPath
    if there
      then pure $ Just configPath
      else do
        let super = parent cwd
        if null (fromAbsDir super) || super == cwd
          then pure Nothing
          else self super

runApp :: (MonadUnliftIO m, HasCallStack) => RIO App () -> m ()
runApp act = do
  mpath <- findYamlConfigPathIn =<< getCurrentDir
  let configBasePath = parent <$> mpath
  path <- maybe getGlobalConfigFilePath pure mpath
  config <- Y.decodeFileThrow $ fromAbsFile path
  logOpts <-
    logOptionsHandle stdout True
      <&> setLogUseLoc False
  withLogFunc logOpts $ \logger ->
    withResourceMap $ \resourceMap ->
      runRIO App {..} $ act `catchAny` reporter

reporter :: (HasCallStack) => SomeException -> RIO App a
reporter exc = do
  logError $ fromString (displayException exc)
  maybe exitFailure exitWith (fromException exc)

data App = App
  { config :: ScaffoldConfig
  , configBasePath :: Maybe (Path Abs Dir)
  , resourceMap :: ResourceMap
  , logger :: LogFunc
  }
  deriving (Generic)

instance HasLogFunc App where
  logFuncL = #logger

instance HasResourceMap App where
  resourceMapL = #resourceMap