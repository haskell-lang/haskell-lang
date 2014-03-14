{-# LANGUAGE OverloadedStrings #-}

-- | CSS theme.

module HL.C.Theme where

import HL.C hiding (Text)
import HL.V.Theme

import Data.Text.Lazy (Text)
import Language.CSS

-- | Generate CSS from theme.
getThemeR :: C TypedContent
getThemeR =
  respondSource "text/css" (sendChunk themeCss)

-- | The theme's CSS.
themeCss :: Text
themeCss =
  renderCSS (runCSS theme)
