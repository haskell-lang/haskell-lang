-- | News page controller.

module HL.Controller.News where

import HL.Foundation
import HL.View.News

-- | News controller.
getNewsR :: Handler Html
getNewsR = blaze newsV
