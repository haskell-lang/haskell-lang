{-# LANGUAGE OverloadedStrings #-}

-- | View library.

module HL.V
  (module HL.V
  ,module V)
  where

import HL.Foundation as V (Route(..),App,Human(..),Slug(..))
import HL.Static as V
import HL.Types as C

import Control.Monad as V
import Data.Text as V (Text)
import Lucid as V
import Lucid.Bootstrap as V
import Yesod.Lucid as V hiding (toHtml)

todo :: Term a r => a -> r
todo = termWith "div" [class_ "muted"]
