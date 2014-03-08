{-# LANGUAGE OverloadedStrings #-}

-- | Wiki page controller.

module HL.Controller.Wiki where

import           HL.Foundation
import           HL.View.Wiki

import           Blaze (renderHtml)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text,unpack)
import           Network.HTTP.Conduit
import           Prelude hiding (readFile)
import           Text.Pandoc.Readers.MediaWiki
import           Text.Pandoc.Writers.HTML
import           Text.XML
import           Text.XML.Cursor

-- | Wiki controller.
getWikiR :: Text -> Handler Html
getWikiR name =
  do html <- liftIO (getWikiPageHtml name)
     blaze (wikiV html)

-- | Get the MediaWiki markup of a wiki page and then convert it to
-- HTML.
getWikiPageHtml :: Text -> IO (Either Text (Text,Html))
getWikiPageHtml article =
  do request <- parseUrl ("http://www.haskell.org/haskellwiki/Special:Export/" <> unpack article)
     withManager $ \manager -> do
       response <- http request manager
       doc <- responseBody response $$+- sinkDoc def
       case parse doc of
         Nothing -> return (Left "Unable to parse XML from haskell.org.")
         Just (title,text) -> return (Right (title,writeHtml def text))
  where
    parse doc =
      do let cursor = fromDocument doc
         title <- listToMaybe (getTitle cursor)
         text <- listToMaybe (getText cursor)
         return (title,readMediaWiki def (unpack text))
    name n =
      Name {nameLocalName = n
           ,nameNamespace = Just "http://www.mediawiki.org/xml/export-0.6/"
           ,namePrefix = Nothing}
    getText cursor =
      element (name "mediawiki") cursor >>=
      descendant >>=
      element (name "page") >>=
      descendant >>=
      element (name "text") >>=
      descendant >>=
      content
    getTitle cursor =
      element (name "mediawiki") cursor >>=
      descendant >>=
      element (name "page") >>=
      descendant >>=
      element (name "title") >>=
      descendant >>=
      content
