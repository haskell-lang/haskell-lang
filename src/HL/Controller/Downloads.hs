-- | Downloads page controller.

module HL.Controller.Downloads where

import HL.Foundation
import HL.View.Downloads

-- | Downloads controller.
getDownloadsR :: Handler Html
getDownloadsR = blaze downloadsV
