{-# LANGUAGE TemplateHaskell #-}

module Development.Scaffold.Cabal.App (
  versionInfo,
  defaultMain,
  defaultMainWith,
  Cmd (..),
  appOptsP,
) where

import Control.Applicative ((<**>), (<|>))
import Control.Monad ((<=<))
import Data.Version (Version, showVersion)
import Development.Scaffold.Cabal
import GitHash
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftData)
import qualified Options.Applicative as Opt
import Paths_cabal_scaffold

data Cmd
  = New ProjectOptions
  | Expand ExpandOptions
  | Import ImportOptions
  | List
  deriving (Show, Eq, Ord)

data VersionInfo = VersionInfo
  { packageVersion :: Version
  , gitInfo :: Either String GitInfo
  }
  deriving (Show)

versionInfo :: Code Q VersionInfo
versionInfo =
  [||
  VersionInfo
    { packageVersion = $$(unsafeCodeCoerce $ liftData version)
    , gitInfo = $$tGitInfoCwdTry
    }
  ||]

shortVersion :: VersionInfo -> Opt.Parser (a -> a)
shortVersion vinfo =
  Opt.infoOption (showVersion $ packageVersion vinfo) $
    mconcat
      [ Opt.long "numeric-version"
      , Opt.help "Show numeric version"
      ]

longVersion :: VersionInfo -> Opt.Parser (a -> a)
longVersion vinfo =
  Opt.infoOption str $
    mconcat
      [ Opt.long "version"
      , Opt.short 'V'
      , Opt.help "Show version info"
      ]
  where
    str = "cabal-scaffold " <> showVersion (packageVersion vinfo) <> git
    git =
      case gitInfo vinfo of
        Left {} -> ""
        Right gi -> " (commit: " <> giHash gi <> ")"

appOptsP :: VersionInfo -> Opt.ParserInfo Cmd
appOptsP vinfo = Opt.info (p <**> longVersion vinfo <**> shortVersion vinfo <**> Opt.helper) $ Opt.progDesc theProgDesc
  where
    theProgDesc = "cabal-scaffold Cabal project scaffold with Stackage Snapshots"
    p =
      Opt.hsubparser defCommands
        <|> Opt.hsubparser (externalCommandProxy <> Opt.internal)
    defCommands =
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
        , Opt.command "list" $
            Opt.info (pure List) $
              Opt.progDesc "List all the available templates"
        , Opt.command "ls" $
            Opt.info (pure List) $
              Opt.progDesc "Alias for list"
        ]

    externalCommandProxy =
      Opt.command
        "scaffold"
        ( Opt.info
            (Opt.hsubparser defCommands)
            $ Opt.progDesc theProgDesc
        )

defaultMain :: VersionInfo -> IO ()
defaultMain = defaultMainWith <=< Opt.customExecParser (Opt.prefs Opt.subparserInline) . appOptsP

defaultMainWith :: Cmd -> IO ()
defaultMainWith cmd =
  case cmd of
    New opts -> runApp $ newProject opts
    Expand opts -> runApp $ expandTemplate opts
    Import opts -> runApp $ importTemplate opts
    List -> runApp listTemplates
