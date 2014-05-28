{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Wiki page view.

module HL.V.Wiki where

import HL.V
import HL.V.Code
import HL.V.Template

import Data.List (isPrefixOf)
import Data.Text (unpack,pack)
import Language.Haskell.HsColour.CSS (hscolour)
import Prelude hiding (readFile)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML

-- | Wiki view.
wikiV :: (Route App -> Text) -> Either Text (Text,Pandoc) -> FromSenza App
wikiV urlr result =
  template
    ([WikiHomeR] ++
     [WikiR n | Right (n,_) <- [result]])
    (case result of
       Left{} -> "Wiki error!"
       Right (t,_) -> t)
    (\_ ->
       container
         (row
            (span12
               []
               (case result of
                  Left err ->
                    do h1 [] "Wiki page retrieval problem!"
                       p [] (toHtml err)
                  Right (t,pan) ->
                    do h1 [] (toHtml t)
                       writeHtml writeOptions (cleanup urlr pan)))))
  where cleanup url = highlightBlock . highlightInline . relativize url
        writeOptions = def { writerTableOfContents = True }

-- | Make all wiki links use the wiki route.
relativize :: (Route App -> Text) -> Pandoc -> Pandoc
relativize url = walk links
  where links asis@(Link is (ref,t))
          | isPrefixOf "http://" ref || isPrefixOf "https://" ref = asis
          | otherwise = Link is (unpack (url (WikiR (pack ref))),t)
        links x = x

-- | Highlight code blocks and inline code samples with a decent
-- Haskell syntax highlighter.
highlightBlock :: Pandoc -> Pandoc
highlightBlock = walk codes
  where codes (CodeBlock ("",["haskell"],[]) text) =
          RawBlock "html" (hscolour False text)
        codes x = x

-- | Highlight code blocks and inline code samples with a decent
-- Haskell syntax highlighter.
highlightInline :: Pandoc -> Pandoc
highlightInline = walk codes
  where codes (Code ("",["haskell"],[]) text) =
          RawInline "html" (preToCode (hscolour False text))
        codes x = x
