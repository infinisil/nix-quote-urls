module Options
  ( getOptions
  , Options(..)
  , Mode(..)
  ) where

import           Options.Applicative

data Options = Options
  { optMode  :: Mode
  , optPaths :: [FilePath]
  }

data Mode = Fixup
          deriving Show

parser :: Parser Options
parser = hsubparser
   ( command "fixup" (info (forMode Fixup) (progDesc "Fixup files/directories by quoting all urls"))
   )

forMode :: Mode -> Parser Options
forMode mode = Options mode
  <$> some (argument str (metavar "PATH.."))

options :: ParserInfo Options
options = info (parser <**> helper)
   ( fullDesc
  <> progDesc "Verify or fixup unquoted URLs in PATH"
  <> header "nix-quote-urls - a helper against unquoted Nix urls" )

getOptions :: IO Options
getOptions = customExecParser p options where
  p = prefs showHelpOnEmpty
