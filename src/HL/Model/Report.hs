{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Read the Haskell report from file, do necessary transformations
-- to get a reasonable page out of it.

module HL.Model.Report where

import           HL.Controller
import           HL.Static

import           Control.Exception
import qualified Data.ByteString as S
import           Data.Char
import qualified Data.Text as T
import           Data.Text.ICU.Convert
import           Lucid
import           System.Directory
import           System.FilePath

-- | Get a report's page. The Haskell report is ISO-8859-1 encoded
-- rather than UTF-8 so we use ICU to decode it from that instead,
-- strip out surrounding HTML tags, and then return it as normal Html.
getReportPage :: Int -> FilePath -> IO (Html ())
getReportPage year path =
  do dir <- getStaticDir
     exists <- doesFileExist (dir </> fp)
     converter <- open "iso-8859-1" (Just True)
     if exists
        then fmap (toHtmlRaw . strip . toUnicode converter) (S.readFile fp)
        else throw (ReportPageNotFound fp)
  where normalize = filter (\c -> isDigit c || isLetter c || c == '.')
        fp = "report" </> ("haskell" ++ show year) </> normalize path
        strip = T.unlines .
                takeWhile (not . T.isPrefixOf "</body>") .
                drop 2 .
                dropWhile (/="</head><body ") .
                T.lines
