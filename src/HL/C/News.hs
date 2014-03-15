-- | News page controller.

module HL.C.News where

import HL.C
import HL.M.News
import HL.V.News

-- | News controller.
getNewsR :: C Html
getNewsR =
  do html <- getHaskellNews
     blaze (newsV html)
