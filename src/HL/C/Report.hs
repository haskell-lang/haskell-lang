-- | Report page controller.

module HL.C.Report where

import HL.C
import HL.V.Report
import HL.M.Report

-- | Report controller.
getReportR :: Int -> FilePath -> C Html
getReportR year page =
  do content <- io (getReportPage year page)
     senza (reportV year page content)

-- | Default page to go to for the given year.
getReportHomeR :: Int -> C Html
getReportHomeR year =
  redirect (ReportR year "haskell.html")
