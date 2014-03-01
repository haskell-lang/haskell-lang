-- | Home page controller.

module HL.C.Home where

import HL.Foundation
import HL.V.Home

-- | Home controller.
getHomeR :: Handler Html
getHomeR = blaze homeV
