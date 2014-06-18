{-# LANGUAGE ViewPatterns #-}

-- | Code highlighting.

module HL.V.Code where

import HL.V

import Data.Text (unpack)
import Language.Haskell.HsColour.CSS (hscolour)
import Text.Blaze.Html

-- | Some syntax-highlighted code.
haskellPre :: Text -> Senza
haskellPre = preEscapedToHtml . hscolour False . unpack

-- | Some syntax-highlighted code.
haskellCode :: Text -> Senza
haskellCode = preEscapedToHtml . preToCode . hscolour False . unpack

-- | Convert a <pre> tag code sample to <code>.
preToCode :: [Char] -> [Char]
preToCode = codeEl . stripCPre . stripPre
  where stripPre ('<':'p':'r':'e':'>':xs) = xs
        stripPre xs = xs
        stripCPre (reverse -> ('<':'/':'p':'r':'e':'>':xs)) = reverse xs
        stripCPre xs = xs
        codeEl xs = "<code>" ++ xs ++ "</code>"
