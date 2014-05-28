-- | Home page controller.

module HL.C.Home where

import HL.C
import HL.V.Home

-- | Home controller.
getHomeR :: C Html
getHomeR = senza homeV
