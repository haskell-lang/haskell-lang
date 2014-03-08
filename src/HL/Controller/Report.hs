-- | Report page controller.

module HL.Controller.Report where

import HL.Foundation
import HL.View.Report

-- | Report controller.
getReportR :: Int -> Handler Html
getReportR year = blaze reportV
