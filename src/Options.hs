module Options where

import Options.Applicative
import Data.Monoid



data Options = Options
    { searchOpts     :: String
    , docsURLOpts    :: Bool
    , htmlOutputOpts :: Bool
    } deriving Show


parseOpts :: Parser Options
parseOpts = Options
    <$> argument str (metavar "QUERY")
    <*> switch (long "url" <> short 'u' <> help "searchs the local HTML file with the documentation for the query")
    <*> switch (long "html-output" <> short 'h' <> help "parses the documentation and print the html")
