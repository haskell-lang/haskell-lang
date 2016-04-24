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
     converter <- makeConverter
     if exists
        then fmap (toHtmlRaw . strip . toUnicode converter)
                  (S.readFile (dir </> fp))
        else throw (ReportPageNotFound fp)
  where normalize =
          filter (\c -> isDigit c || isLetter c || c == '.')
        fp =
          "report" </>
          ("haskell" ++ show year) </>
          normalize path

-- | Get ALL pages in one big-ass HTML output.
getReportAllPages :: Int -> IO (Html ())
getReportAllPages year =
  do staticDir <- getStaticDir
     let reportDir =
           staticDir </> "report" </>
           ("haskell" ++ show year)
     let files =
           ["li2"] ++
           map (("ch" ++) . show)
               [1 .. 42 :: Int] ++
           ["li3"]
     let pages =
           map (\x -> "haskell" ++ x ++ ".html") files
     converter <- makeConverter
     chunks <-
       mapM (S.readFile . (reportDir </>)) pages
     return (mapM_ (toHtmlRaw . strip . toUnicode converter) chunks)

-- | Strip out everything before and after the body.
strip :: Text -> Text
strip =
  T.unlines .
  takeWhile (not .
             T.isPrefixOf "</body>") .
  drop 2 .
  dropWhile (/= "</head><body ") .
  T.lines

-- | Make a converter appropriate for the report.
makeConverter :: IO Converter
makeConverter = open "iso-8859-1" (Just True)
