-- | Wiki page controller.

module HL.Controller.Wiki where

import HL.Foundation
import HL.View.Wiki

import Data.Text (Text)

-- | Wiki controller.
getWikiR :: Text -> Handler Html
getWikiR name = blaze wikiV
