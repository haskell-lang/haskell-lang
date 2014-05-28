{-# LANGUAGE BangPatterns #-}

-- | Markdown files.

module HL.M.Markdown where

import           HL.C
import           HL.Types

import           Control.Exception
import qualified Data.Text.IO as ST
import qualified Data.Text.Lazy as L
import           System.Directory
import           System.FilePath
import           Text.Markdown

-- | Get the HTML for the given markdown static file.
getMarkdown :: FilePath -> IO Html
getMarkdown name =
  do exists <- doesFileExist fp
     if exists
        then do text <- fmap L.fromStrict (ST.readFile fp)
                let !html = markdown def text
                return html
        else throw (MarkdownFileUnavailable name)
  where fp = "static" </> "markdown" </> name
