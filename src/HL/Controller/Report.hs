-- | Report page controller.

module HL.Controller.Report where

import HL.Controller
import HL.Model.Report
import HL.View
import HL.View.Report

-- | Report controller.
getReportR :: Int -> FilePath -> C (Html ())
getReportR year page =
  do content <- io (getReportPage year page)
     lucid (reportV year page content)

-- | Default page to go to for the given year.
getReportHomeR :: Int -> C (Html ())
getReportHomeR year =
  redirect (ReportR year "haskell.html")
