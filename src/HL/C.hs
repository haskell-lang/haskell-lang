-- | Controller library.

module HL.C
  (module C
  ,App(..)
  ,C
  ,io)
  where

import HL.Foundation (Handler)
import HL.Foundation as C (Route(..),App(..))

import Control.Monad.Extra
import Data.Text as C (Text)
import Yesod as C
import Yesod.Blaze as C

-- | Controller type.
type C = Handler
