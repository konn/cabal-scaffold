module Main (main) where

import Control.Applicative ((<**>))
import Development.Scaffold.Cabal
import qualified Options.Applicative as Opt

data Cmd
  = New ProjectOptions
  | Expand ExpandOptions
  | Import ImportOptions
  deriving (Show, Eq, Ord)

appP :: Opt.ParserInfo Cmd
appP = Opt.info (p <**> Opt.helper) $ Opt.progDesc "Cabal project scaffold with Stackage Snapshots"
  where
    p =
      Opt.hsubparser $
        mconcat
          [ Opt.command
              "new"
              $ Opt.info (New <$> projectOptionsP)
              $ Opt.progDesc "Create New Project"
          , Opt.command
              "expand"
              $ Opt.info (Expand <$> expandOptionsP)
              $ Opt.progDesc "Search and expand the template to the specified directory"
          , Opt.command
              "import"
              $ Opt.info (Import <$> importOptionsP)
              $ Opt.progDesc "Import directory or .hsfiles as a new preset template"
          ]

main :: IO ()
main = do
  cmd <- Opt.execParser appP
  case cmd of
    New opts -> runApp $ newProject opts
    Expand opts -> runApp $ expandTemplate opts
    Import opts -> runApp $ importTemplate opts
