{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Markdown files.

module HL.Model.Markdown where

import           Control.Exception
import qualified Data.ByteString as S
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as L
import           HL.Controller
import           HL.View
import           HL.View.Code
import           System.Directory
import           System.FilePath
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import           Text.Markdown (markdown, msBlockCodeRenderer, def)

-- | Render Markdown to HTML
renderMarkdown :: Markdown -> Html ()
renderMarkdown (Markdown text) =
    toHtmlRaw
        (renderHtml
             (markdown
                  def
                  { msBlockCodeRenderer = renderer
                  }
                  (L.fromStrict text)))
  where
    renderer lang (src,_) =
        if lang == Just "haskell"
            then H.preEscapedToHtml (renderText (haskellPre src))
            else H.pre $ H.toHtml src

-- | Get the HTML for the given markdown static file.
getMarkdown :: FilePath -> IO (Html ())
getMarkdown name =
  do dir <- getStaticDir
     exists <- doesFileExist (dir </> fp)
     if exists
        then do text <- fmap (decodeUtf8With lenientDecode)
                             (S.readFile (dir </> fp))
                let !html = renderMarkdown (Markdown text)
                return html
        else throw (MarkdownFileUnavailable name)
  where fp = "markdown" </> name
