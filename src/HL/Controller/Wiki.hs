{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wiki page controller.

module HL.Controller.Wiki where

import           HL.Foundation
import           HL.View.Wiki

import Text.Pandoc.Options
import           Blaze (renderHtml)
import           Language.Haskell.HsColour.CSS (hscolour)
import           Control.Exception.Lifted (catch)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.List (isPrefixOf)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text,unpack,pack)
import           Network.HTTP.Conduit
import           Prelude hiding (readFile,catch)
import           Text.Pandoc.Definition
import           Text.Pandoc.Readers.MediaWiki
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.HTML
import           Text.XML
import           Text.XML.Cursor

-- | Wiki home (no page specified).
getWikiHomeR :: Handler Html
getWikiHomeR =
  redirect (WikiR "HaskellWiki:Community")

-- | Wiki controller.
getWikiR :: Text -> Handler Html
getWikiR name =
  do url <- getUrlRender
     html <- liftIO (getWikiPageHtml url name)
     blaze (wikiV html)

-- | Get the MediaWiki markup of a wiki page and then convert it to
-- HTML.
getWikiPageHtml url article =
  do request <- parseUrl ("http://www.haskell.org/haskellwiki/Special:Export/" <> unpack article)
     withManager $ \manager -> do
       response <- http request manager
       doc <- catch (fmap Just (responseBody response $$+- sinkDoc def))
                    (\(e::UnresolvedEntityException) -> return Nothing)
       case doc >>= parse of
         Nothing -> return (Left "Unable to parse XML from haskell.org.")
         Just (title,text) -> return (Right (title,writeHtml writeOptions text))
  where
    writeOptions = def { writerTableOfContents = True }
    parse doc =
      do let cursor = fromDocument doc
         title <- listToMaybe (getTitle cursor)
         text <- listToMaybe (getText cursor)
         return (title,(highlightBlock . highlightInline . relativize url)
                       (readMediaWiki def (unpack text)))
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

-- | Make all wiki links use the wiki route.
relativize :: (Route App -> Text) -> Pandoc -> Pandoc
relativize url = walk links
  where links asis@(Link is (ref,title))
          | isPrefixOf "http://" ref || isPrefixOf "https://" ref = asis
          | otherwise = Link is (unpack (url (WikiR (pack ref))),title)
        links x = x

-- | Highlight code blocks and inline code samples with a decent
-- Haskell syntax highlighter.
highlightBlock :: Pandoc -> Pandoc
highlightBlock = walk codes
  where codes (CodeBlock attrs@("",["haskell"],[]) text) =
          RawBlock "html" (hscolour False text)
        codes x = x

-- | Highlight code blocks and inline code samples with a decent
-- Haskell syntax highlighter.
highlightInline :: Pandoc -> Pandoc
highlightInline = walk codes
  where codes (Code attrs@("",["haskell"],[]) text) =
          RawInline "html" (codeEl (stripCPre (stripPre (hscolour False text))))
        codes x = x
        stripPre ('<':'p':'r':'e':'>':xs) = xs
        stripPre xs = xs
        stripCPre (reverse -> ('<':'/':'p':'r':'e':'>':xs)) = reverse xs
        stripCPre xs = xs
        codeEl xs = "<code>" ++ xs ++ "</code>"
