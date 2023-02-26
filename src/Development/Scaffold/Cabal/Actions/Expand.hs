{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.Scaffold.Cabal.Actions.Expand (
  expandTemplate,
  ExpandOptions (..),
  expandOptionsP,
) where

import qualified Data.Bifunctor as Bi
import Data.Generics.Labels ()
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Development.Scaffold.Cabal.Runner
import Development.Scaffold.Cabal.Template
import qualified Options.Applicative as Opt
import Path (filename, fromAbsDir, fromAbsFile, fromRelFile)
import Path.IO (resolveDir')
import Path.IO.Utils (makeSureEmpty)
import RIO
import qualified RIO.FilePath as RawFP
import qualified Streaming.Prelude as S

data ExpandOptions = ExpandOptions
  { template :: String
  , destination :: Maybe FilePath
  }
  deriving (Show, Eq, Ord, Generic)

expandOptionsP :: Opt.Parser ExpandOptions
expandOptionsP = do
  template <- Opt.strArgument (Opt.metavar "TEMPLATE" <> Opt.help "The template to expand")
  destination <-
    Opt.optional $
      Opt.strArgument (Opt.metavar "DIR" <> Opt.help "The directory to put the expanded template")
  pure ExpandOptions {..}

expandTemplate :: ExpandOptions -> RIO App ()
expandTemplate ExpandOptions {..} = do
  logInfo $ "Expanding template: " <> fromString template
  cfg <- view #config
  pth <-
    maybe (throwString $ "Template not found: " <> template) pure
      =<< searchTemplatePath cfg template
  logInfo $ "Expanding tempalte from: " <> fromString (fromAbsFile pth)
  let dest0 = RawFP.takeBaseName $ fromRelFile $ filename pth
  dest <- resolveDir' dest0
  logInfo $ "Template will be expanded to: " <> fromString (fromAbsDir dest)
  makeSureEmpty dest
  readTemplateFile pth
    & decodeTemplate
    & S.map (Bi.second $ LT.toStrict . LT.decodeUtf8)
    & sinkToDir dest
  logInfo $ "Template expanded to: " <> fromString (fromAbsDir dest)
