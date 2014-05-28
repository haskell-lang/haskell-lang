-- | Community page controller.

module HL.C.Community where

import HL.C
import HL.V.Community

-- | Community controller.
getCommunityR :: C Html
getCommunityR = senza communityV
