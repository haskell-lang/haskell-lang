-- | Donate page controller.

module HL.Controller.Donate where

import HL.Controller
import HL.View.Donate
import HL.View

-- | Donate controller.
getDonateR :: C (Html ())
getDonateR = lucid donateV
