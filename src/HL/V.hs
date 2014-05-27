-- | View library.

module HL.V
  (module V)
  where

import HL.Foundation as V (Route(..),App)
import HL.Static as V

import Control.Monad as V
import Data.Text as V (Text)
import Prelude as V hiding (span,head,min,max,id,div)
import Senza as V
import Senza.Bootstrap as V
import Yesod.Senza as V
