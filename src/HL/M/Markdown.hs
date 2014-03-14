{-# LANGUAGE BangPatterns #-}

-- | Markdown files.

module HL.M.Markdown where

import           HL.C
import           HL.Types

import           Control.Exception
import qualified Data.Text.Lazy.IO as LT
import           System.Directory
import           System.FilePath
import           Text.Markdown

-- | Get the HTML for the given markdown static file.
getMarkdown :: FilePath -> C Html
getMarkdown name =
  do exists <- io (doesFileExist fp)
     if exists
        then do text <- io (LT.readFile fp)
                let !html = markdown def text
                return html
        else throw (MarkdownFileUnavailable name)
  where fp = "static" </> "markdown" </> name
