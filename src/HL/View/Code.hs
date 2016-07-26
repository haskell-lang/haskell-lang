{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Code highlighting.

module HL.View.Code where

import HL.View

import Data.Text (unpack)
import Language.Haskell.HsColour.CSS (hscolour)

-- | Some syntax-highlighted code.
haskellPre :: Monad m => Text -> HtmlT m ()
haskellPre = toHtmlRaw . hscolour False 1 . unpack

-- | Some syntax-highlighted code.
rejectedHaskellPre
    :: Monad m
    => Text -> Text -> HtmlT m ()
rejectedHaskellPre msg = wrap . toHtmlRaw . hscolour False 1 . unpack
  where
    wrap
        :: Monad m
        => HtmlT m () -> HtmlT m ()
    wrap inner =
        div_
            [class_ "rejected-code"]
            (do span_ [class_ "rejected-note"] (toHtml msg)
                inner)

-- | Some syntax-highlighted code.
haskellCode
    :: Monad m
    => Text -> HtmlT m ()
haskellCode = toHtmlRaw . preToCode . hscolour False 1 . unpack

-- | Convert a <pre> tag code sample to <code>.
preToCode :: [Char] -> [Char]
preToCode = codeEl . stripCPre . stripPre
  where stripPre ('<':'p':'r':'e':'>':xs) = xs
        stripPre xs = xs
        stripCPre (reverse -> ('<':'/':'p':'r':'e':'>':xs)) = reverse xs
        stripCPre xs = xs
        codeEl xs = "<code>" ++ xs ++ "</code>"
