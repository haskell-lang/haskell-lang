-- | News page controller.

module HL.C.News where

import HL.C
import HL.V.News

-- | News controller.
getNewsR :: C Html
getNewsR = blaze newsV
