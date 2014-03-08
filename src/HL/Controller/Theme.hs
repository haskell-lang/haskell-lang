{-# LANGUAGE OverloadedStrings #-}

-- | CSS theme.

module HL.Controller.Theme where

import HL.Foundation
import HL.View.Theme

import Data.Text.Lazy (Text)
import Language.CSS

-- | Generate CSS from Clay theme.
getThemeR :: Handler TypedContent
getThemeR =
  respondSource "text/css" (sendChunk themeCss)

themeCss :: Text
themeCss = renderCSS (runCSS theme)
