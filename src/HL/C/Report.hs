-- | Report page controller.

module HL.C.Report where

import HL.C
import HL.V.Report

-- | Report controller.
getReportR :: Int -> C Html
getReportR _ = blaze reportV
