{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Read the Haskell report from file, do necessary transformations
-- to get a reasonable page out of it.

module HL.M.Report where

import           HL.C
import           HL.Types

import           Control.Applicative ((<|>))
import           Control.Exception
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString as S
import           Data.Char
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.ICU.Convert
import           System.Directory
import           System.FilePath


-- | Get a report's page. The Haskell report is ISO-8859-1 encoded
-- rather than UTF-8 so we use ICU to decode it from that instead,
-- strip out surrounding HTML tags, and then return it as normal Html.
getReportPage :: Int -> FilePath -> IO Html
getReportPage year path =
  do exists <- doesFileExist fp
     converter <- open "iso-8859-1" (Just True)
     if exists
        then fmap (preEscapedToMarkup . strip . toUnicode converter) (S.readFile fp)
        else throw (ReportPageNotFound fp)
  where normalize = filter (\c -> isDigit c || isLetter c || c == '.')
        fp = "static" </> "report" </> ("haskell" ++ show year) </> normalize path
        strip = T.unlines .
                takeWhile (not . T.isPrefixOf "</body>") .
                drop 2 .
                dropWhile (/="</head><body ") .
                T.lines

-- | Rather than parsing the HTML, which is slower, we simply strip
-- out any text until we see <body> and keep until we see </body>.
stripWrapper :: Text -> Text
stripWrapper =
  either (const "Unable to clean up report's HTML.") id .
  P.parseOnly dropUntilBody
  where dropUntilBody =
          do P.skipWhile (/='<')
             P.char '<'
             body <- fmap (const True) (P.string "body") <|> return False
             if body
                then do P.skipSpace
                        P.char '>'
                        takeUntilEndBody
                else dropUntilBody
        takeUntilEndBody =
          do content <- P.takeWhile (/='<')
             body <- fmap (const True) (P.string "/body") <|> return False
             if body
                then return content
                else do rest <- takeUntilEndBody
                        return (content <> rest)
