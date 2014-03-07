-- | Community page controller.

module HL.Controller.Community where

import HL.Foundation
import HL.View.Community

-- | Community controller.
getCommunityR :: Handler Html
getCommunityR = blaze communityV
