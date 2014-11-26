{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wiki page controller.

module HL.C.Wiki where

import HL.C
import HL.M.Wiki
import HL.V
import HL.V.Wiki

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
