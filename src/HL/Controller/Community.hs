-- | Community page controller.

module HL.Controller.Community where

import HL.Controller
import HL.View.Community
import HL.View

-- | Community controller.
getCommunityR :: C (Html ())
getCommunityR = lucid communityV
