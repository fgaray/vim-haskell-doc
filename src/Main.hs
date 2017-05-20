{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Haddock
import Options
import Options.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    options <- execParser opts
    run options
    where
        opts = info (parseOpts <**> helper)
            (  fullDesc
            <> progDesc "Searchs a function or datatype in Haddock and prints the documentation"
            )


run :: Options -> IO ()
run Options{..} = do
    let query = T.pack searchOpts
    if docsURLOpts
        then do
            mayUrl <- docURL query
            case mayUrl of
                Nothing -> putStrLn "Not found"
                Just url -> T.putStrLn url
        else do
            result <- if htmlOutputOpts then searchHtml query else searchText query
            case result of
                Left err -> putStrLn err
                Right txt -> T.putStrLn txt

