module HL.Model.Tutorial
    ( getTutorials
    ) where

import qualified Data.ByteString as S
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import HL.Types
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeBaseName, takeExtension)

getTutorials :: IO (Map Text Tutorial)
getTutorials = do
    files <-
        getDirectoryContents root >>=
        filterM doesFileExist .
        filter
            (\fp ->
                  takeExtension fp == ".md") .
        map (root </>)
    fmap Map.fromList (mapM toPair files)
  where
    root = "static/tutorial"
    filterM _ [] = return []
    filterM f (x:xs) = do
        res <- f x
        if res
            then fmap (x :) (filterM f xs)
            else filterM f xs
    toPair fp = do
        bs <- S.readFile fp
        let text = decodeUtf8With lenientDecode bs
            title = getTitleSimple text
            tut =
                Tutorial
                { tutorialTitle = title
                , tutorialContent = Markdown text
                }
        return (slug, tut)
      where
        slug = T.pack (takeBaseName fp)

-- | A simple approach to getting the title from a Markdown file
getTitleSimple :: Text -> Text
getTitleSimple = T.strip . T.takeWhile (/= '\n') . T.dropWhile (== '#')
