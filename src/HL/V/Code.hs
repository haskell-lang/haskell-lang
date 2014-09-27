{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Code highlighting.

module HL.V.Code where

import HL.V
import Data.Text (unpack)
import Language.Haskell.HsColour.CSS (hscolour)

-- | Some syntax-highlighted code.
haskellPre :: Text -> Html
haskellPre = preEscapedToHtml . hscolour False . unpack

-- | Some syntax-highlighted code.
rejectedHaskellPre :: Text -> Text -> Html
rejectedHaskellPre msg = wrap . preEscapedToHtml . hscolour False . unpack
  where wrap inner =
          HL.V.div !. "rejected-code" $
            do HL.V.span !. "rejected-note" $ toHtml msg
               inner

-- | Some syntax-highlighted code.
haskellCode :: Text -> Html
haskellCode = preEscapedToHtml . preToCode . hscolour False . unpack

-- | Convert a <pre> tag code sample to <code>.
preToCode :: [Char] -> [Char]
preToCode = codeEl . stripCPre . stripPre
  where stripPre ('<':'p':'r':'e':'>':xs) = xs
        stripPre xs = xs
        stripCPre (reverse -> ('<':'/':'p':'r':'e':'>':xs)) = reverse xs
        stripCPre xs = xs
        codeEl xs = "<code>" ++ xs ++ "</code>"
