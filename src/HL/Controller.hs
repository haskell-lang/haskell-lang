-- | Controller library.

module HL.Controller
  (module C
  ,App(..)
  ,C
  ,io)
  where

import HL.Foundation (Handler)
import HL.Foundation as C (Route(..))
import HL.Types as C

import Control.Monad.Extra
import Data.Text as C (Text)
import Yesod as C hiding (Html,toHtml)
import Yesod.Lucid as C

-- | Controller type.
type C = Handler
