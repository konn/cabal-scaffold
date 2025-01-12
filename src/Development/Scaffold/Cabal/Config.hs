{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.Scaffold.Cabal.Config (
  ScaffoldConfig (..),
  Defaults (..),
  Params (..),
) where

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JKM
import qualified Data.Text as T
import Development.Scaffold.Cabal.Snapshots
import GHC.Generics

data ScaffoldConfig = ScaffoldConfig
  { templateDirs :: [FilePath]
  , hpack :: Bool
  , defaults :: Defaults
  , params :: Params
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Defaults = Defaults
  { template :: Maybe String
  , snapshot :: PartialSnapshotName
  , projectFile :: !(Maybe Bool)
  }
  deriving (Show, Eq, Ord, Generic)

-- deriving anyclass (FromJSON, ToJSON)

defaultsOpts :: J.Options
defaultsOpts = J.defaultOptions {J.omitNothingFields = True, J.fieldLabelModifier = J.camelTo2 '-'}

instance FromJSON Defaults where
  parseJSON = J.genericParseJSON defaultsOpts

instance ToJSON Defaults where
  toJSON = J.genericToJSON defaultsOpts

data Params = Params
  { authorName :: T.Text
  , authorEmail :: T.Text
  , githubUsername :: Maybe T.Text
  , copyright :: Maybe T.Text
  , rawParams :: JKM.KeyMap J.Value
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Params where
  parseJSON = J.withObject "params" $ \obj -> do
    authorName <- obj .: "author-name"
    authorEmail <- obj .: "author-email"
    githubUsername <- obj .:? "github-username"
    copyright <- obj .:? "copyright"
    let rawParams =
          JKM.delete "author-name" $
            JKM.delete "author-email" $
              JKM.delete "github-username" $
                JKM.delete "copyright" obj
    pure Params {..}

instance ToJSON Params where
  toJSON Params {..} =
    J.Object $
      JKM.insert "author-name" (J.toJSON authorName) $
        JKM.insert "author-email" (J.toJSON authorEmail) $
          maybe id (JKM.insert "github-username" . J.toJSON) githubUsername $
            maybe id (JKM.insert "copyright" . J.toJSON) copyright rawParams
