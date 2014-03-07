-- | Home page controller.

module HL.Controller.Home where

import HL.Foundation
import HL.View.Home

-- | Home controller.
getHomeR :: Handler Html
getHomeR = blaze homeV
