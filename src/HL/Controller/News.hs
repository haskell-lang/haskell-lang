-- | News page controller. This page simply downloads from
-- haskellnews.org which already has a pre-prepared page of news to
-- display.

module HL.Controller.News where

import HL.Controller
import HL.Model.News
import HL.View
import HL.View.News

-- | News controller.
getNewsR :: C (Html ())
getNewsR =
  do html <- io getHaskellNews
     lucid (newsV html)
