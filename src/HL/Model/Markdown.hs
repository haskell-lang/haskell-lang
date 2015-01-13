{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Markdown files.

module HL.Model.Markdown where

import           HL.Controller

import           HL.View
import           HL.View.Code


import           Control.Exception
import qualified Data.Text.IO as ST
import qualified Data.Text.Lazy as L
import           System.Directory
import           System.FilePath
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Markdown

-- | Get the HTML for the given markdown static file.
getMarkdown :: FilePath -> IO (Html ())
getMarkdown name =
  do dir <- getStaticDir
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
