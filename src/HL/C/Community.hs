-- | Community page controller.

module HL.C.Community where

import HL.C
import HL.V.Community
import HL.V

-- | Community controller.
getCommunityR :: C (Html ())
getCommunityR = lucid communityV
