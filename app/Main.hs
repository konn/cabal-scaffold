module Main (main) where

import Control.Applicative ((<**>))
import Data.Text (Text)
import Development.Scaffold.Cabal
import qualified Options.Applicative as Opt

data Cmd
  = New ProjectOptions
  | Expand String FilePath
  | Import Text FilePath
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
              $ Opt.info expandP
              $ Opt.progDesc "Search and expand the template to the specified directory"
          , Opt.command
              "import"
              $ Opt.info importP
              $ Opt.progDesc "Import directory or .hsfiles as a new preset template"
          ]

expandP :: Opt.Parser Cmd
expandP =
  Expand
    <$> Opt.strArgument (Opt.metavar "TEMPLATE" <> Opt.help "The template to expand")
    <*> Opt.strArgument (Opt.metavar "DIR" <> Opt.help "The directory to put the expanded template")

importP :: Opt.Parser Cmd
importP =
  Import
    <$> Opt.strArgument (Opt.metavar "TEMPLATE_NAME" <> Opt.help "The name ofThe new template")
    <*> Opt.strArgument (Opt.metavar "PATH" <> Opt.help "The path to the hsfiles tempalte-file or directory to import as a template")

main :: IO ()
main = do
  cmd <- Opt.execParser appP
  case cmd of
    New opts -> runApp $ newProject opts
    Expand tmplt out -> runApp $ decompressTemplate tmplt out
    Import name src -> runApp $ importTemplate name src
