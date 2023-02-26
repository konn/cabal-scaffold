{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}

module Development.Scaffold.Cabal (
  runApp,
  App (..),
  module Development.Scaffold.Cabal,
) where

import Development.Scaffold.Cabal.Actions.Expand as Development.Scaffold.Cabal
import Development.Scaffold.Cabal.Actions.Import as Development.Scaffold.Cabal
import Development.Scaffold.Cabal.Actions.List as Development.Scaffold.Cabal
import Development.Scaffold.Cabal.Actions.New as Development.Scaffold.Cabal
import Development.Scaffold.Cabal.Runner
