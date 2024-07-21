{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.Scaffold.Cabal.App (defaultMain, versionInfo)

main :: IO ()
main = defaultMain $$(versionInfo)
