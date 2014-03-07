-- | Documentation page controller.

module HL.Controller.Documentation where

import HL.Foundation
import HL.View.Documentation

-- | Documentation controller.
getDocumentationR :: Handler Html
getDocumentationR = blaze documentationV
