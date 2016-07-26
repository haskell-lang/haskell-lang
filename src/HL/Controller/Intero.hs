{-# LANGUAGE OverloadedStrings #-}

-- | Intero page.

module HL.Controller.Intero where

import HL.Controller
import HL.Controller.Html
import HL.View

-- | Intero page.
getInteroR :: C (Html ())
getInteroR =
  htmlPage "Intero for Emacs" "intero.html"
