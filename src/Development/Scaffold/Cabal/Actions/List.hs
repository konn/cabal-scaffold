{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Development.Scaffold.Cabal.Actions.List (listTemplates) where

import Control.Lens (iforM_)
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Monoid (Dual (..))
import qualified Data.Text as T
import Development.Scaffold.Cabal.Runner
import Development.Scaffold.Cabal.Template
import Path
import Path.IO (listDir)
import RIO

listTemplates :: RIO App ()
listTemplates = do
  dirs <- listTemplateDirs =<< view #config
  Dual dic <- foldMap (fmap (Dual . asDic . snd) . listDir) dirs
  logInfo $ displayShow (Map.size dic) <> " template(s) found."
  iforM_ dic $ \name dest ->
    logInfo $ display name <> ": " <> fromString (fromAbsFile dest)
  pure ()

asDic :: [Path Abs File] -> Map Text (Path Abs File)
asDic =
  Map.fromList
    . mapMaybe
      ( \p ->
          (,p) <$> T.stripSuffix ".hsfiles" (T.pack $ fromRelFile $ filename p)
      )
