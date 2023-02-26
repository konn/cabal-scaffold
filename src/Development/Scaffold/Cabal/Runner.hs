{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Scaffold.Cabal.Runner (runApp, App (..)) where

import Data.Generics.Labels ()
import qualified Data.Yaml as Y
import Development.Scaffold.Cabal.Config
import Development.Scaffold.Cabal.Constants
import Path
import RIO
import RIO.Orphans (HasResourceMap (..), ResourceMap, withResourceMap)

runApp :: (MonadUnliftIO m, HasCallStack) => RIO App () -> m ()
runApp act = do
  config <- Y.decodeFileThrow . fromAbsFile =<< getGlobalConfigFilePath
  logOpts <- logOptionsHandle stdout True
  withLogFunc logOpts $ \logger ->
    withResourceMap $ \resourceMap ->
      runRIO App {..} $ act `catchAny` reporter

reporter :: HasCallStack => SomeException -> RIO App a
reporter exc = do
  logError $ fromString (displayException exc)
  maybe exitFailure exitWith (fromException exc)

data App = App
  { config :: ScaffoldConfig
  , resourceMap :: ResourceMap
  , logger :: LogFunc
  }
  deriving (Generic)

instance HasLogFunc App where
  logFuncL = #logger

instance HasResourceMap App where
  resourceMapL = #resourceMap