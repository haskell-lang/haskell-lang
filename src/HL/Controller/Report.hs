-- | Report page controller.

module HL.Controller.Report where

import HL.Controller
import HL.Foundation
import HL.Model.Report
import HL.View
import HL.View.Report

-- | Report controller.
getReportNodeR :: Int -> FilePath -> C (Html ())
getReportNodeR year page =
  do content <- io (getReportPage year page)
     lucid (reportNodeV Node year content)

-- | Default page to go to for the given year.
getReportModeR :: Mode -> Int -> C (Html ())
getReportModeR mode year =
  case mode of
    Node -> redirect (ReportNodeR year "haskell.html")
    Mono -> do content <- io (getReportAllPages year)
               lucid (reportNodeV mode year content)

-- | Show the report choices.
getReportR :: C (Html ())
getReportR = lucid reportV
