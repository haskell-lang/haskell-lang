{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wiki page controller.

module HL.Controller.Wiki where

import HL.Controller
import HL.Model.Wiki
import HL.View
import HL.View.Wiki

-- | Wiki home (no page specified).
getWikiHomeR :: C (Html ())
getWikiHomeR =
  redirect (WikiR "HaskellWiki:Community")

-- | Wiki controller.
getWikiR :: Text -> C (Html ())
getWikiR name =
  do url <- getUrlRender
     result <- io (getWikiPage name)
     lucid (wikiV url result)
