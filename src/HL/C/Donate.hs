-- | Donate page controller.

module HL.C.Donate where

import HL.C
import HL.V.Donate
import HL.V

-- | Donate controller.
getDonateR :: C (Html ())
getDonateR = lucid donateV
