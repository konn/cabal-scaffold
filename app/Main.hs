module Main (main) where

import Control.Applicative ((<**>))
import Development.Scaffold.Cabal
import qualified Options.Applicative as Opt

newtype Cmd = New ProjectOptions
  deriving (Show, Eq, Ord)

appP :: Opt.ParserInfo Cmd
appP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Cabal project scaffold with Stackage Snapshots"
  where
    p =
      Opt.hsubparser $
        Opt.command
          "new"
          ( Opt.info (New <$> projectOptionsP) $
              Opt.progDesc "Create New Project"
          )

main :: IO ()
main = do
  cmd <- Opt.execParser appP
  case cmd of
    New opts -> runApp $ newProject opts
