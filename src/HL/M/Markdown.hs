{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Markdown files.

module HL.M.Markdown where

import           HL.C
import           HL.V.Code
import           HL.V

import           Paths_hl

import           Control.Exception
import qualified Data.Text.IO as ST
import qualified Data.Text.Lazy as L
import           System.Directory
import           System.FilePath
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Renderer.Text (renderHtml)
import           Text.Markdown

-- | Get the HTML for the given markdown static file.
getMarkdown :: FilePath -> IO (Html ())
getMarkdown name =
  do dir <- getDataFileName "static"
     exists <- doesFileExist (dir </> fp)
     if exists
        then do text <- fmap L.fromStrict (ST.readFile (dir </> fp))
                let !html = renderHtml (markdown def { msBlockCodeRenderer = renderer } text)
                return (toHtmlRaw html)
        else throw (MarkdownFileUnavailable name)
  where fp = "markdown" </> name
        renderer lang (src,_) =
          if lang == Just "haskell"
             then H.preEscapedToHtml (renderText (haskellPre src))
             else H.pre $ H.toHtml src
