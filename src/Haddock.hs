{-# LANGUAGE OverloadedStrings #-}
module Haddock where

import System.Process
import Control.Monad
import Safe
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List
import Data.Monoid
import Data.Char


-- | Searchs a function or datatype in the local haddock documentation. If there
-- is no documentation, then Left is returned
search :: Text -> IO (Either String [Tag Text])
search srch = do
    mod <- searchModule srch
    case mod of
        Nothing -> return . Left $ "Function or datatype not found"
        Just mod' -> do
            docs <- liftM T.unpack $ searchDocs mod'
            html <- T.readFile docs
            let name = toHaddockId srch
            return . Right $ parseHTML name html


-- | Search for a function or datatype in the haddock documentation returning the
-- text of that section
searchText :: Text -> IO (Either String Text)
searchText txt = search txt >>= (return . fmap extractText)

-- | Search for a function or datatype in the haddock documentation returning the
-- html of that section
searchHtml :: Text -> IO (Either String Text)
searchHtml txt = search txt >>= (return . fmap renderTags)

-- | Haddok
toHaddockId :: Text -> Text
toHaddockId txt = if isDataType txt then "t:" <> txt else "v:" <> txt


docsDir :: IO String
docsDir = liftM (head . lines) $ readProcess "stack" ["path", "--snapshot-doc-root"] ""

searchDocs :: String -> IO Text
searchDocs mod = do
    dir <- docsDir
    let mod' = map (\x -> if x == '.' then '-' else x) mod
    docs <- readProcess "find" [dir, "-iname", mod' ++ ".html", "-not", "-path", "*src*"] ""
    return . T.strip . T.pack $ docs

searchSrc :: String -> IO Text
searchSrc mod = do
    dir <- docsDir
    let mod' = map (\x -> if x == '.' then '-' else x) mod
    docs <- readProcess "find" [dir, "-iname", mod' ++ ".html", "-path", "*src*"] ""
    return . T.strip . T.pack $ docs

docURL :: Text -> IO (Maybe Text)
docURL item = do
    mod <- searchModule item
    case mod of
        Nothing -> return Nothing
        Just mod' -> do
            url <- searchDocs mod'
            let id = toHaddockId item
            return . Just $ url


searchModule :: Text -> IO (Maybe String)
searchModule function = do
    mod <- liftM (fmap (headMay . words) . headMay . lines) $ readProcess "stack" ["hoogle", T.unpack function] ""
    return $ join mod


parseHTML :: Text -> Text -> [Tag Text]
parseHTML item file =
    let tags = parseTags file
    in searchTag tags

    where
        searchTag :: [Tag Text] -> [Tag Text]
        searchTag = takeWhile (not . isTagEnd) . dropWhile (not . isTagElement)

        findTag :: [Tag Text] -> Tag Text -> [Tag Text]
        findTag [] x = if isTagElement x then [x] else []
        findTag xs x = x : xs

        isTagEnd :: Tag Text -> Bool
        isTagEnd (TagOpen "div" attrs) = find (\(typ, val) -> typ == "class" && val == "top") attrs /= Nothing
        isTagEnd _ = False

        isTagElement :: Tag Text -> Bool
        isTagElement (TagOpen "a" attrs) = find (\(typ, val) -> typ == "id" && val == item) attrs /= Nothing
        isTagElement _ = False



extractText :: [Tag Text] -> Text
extractText = foldl' extract mempty
    where
        extract :: Text -> Tag Text -> Text
        extract acc (TagText txt) = acc <> " " <> txt
        extract acc _ = acc


isDataType :: Text -> Bool
isDataType txt
    | T.null txt = False
    | otherwise  = isUpper . T.head $ txt
