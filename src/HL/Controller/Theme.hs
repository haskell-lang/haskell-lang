{-# LANGUAGE OverloadedStrings #-}

-- | CSS theme.

module HL.Controller.Theme where

import HL.Foundation
import HL.View.Theme

import Data.Conduit
import Data.Conduit.List as CL
import Data.Text.Lazy (Text)
import Language.CSS

-- | Generate CSS from Clay theme.
getThemeR :: Handler TypedContent
getThemeR =
  respondSource "text/css"
                (sendChunk (renderCSS (runCSS theme)))
