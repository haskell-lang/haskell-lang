-- | News page controller. This page simply downloads from
-- haskellnews.org which already has a pre-prepared page of news to
-- display.

module HL.C.News where

import HL.C
import HL.M.News
import HL.V.News

-- | News controller.
getNewsR :: C Html
getNewsR =
  do html <- io getHaskellNews
     blaze (newsV html)
