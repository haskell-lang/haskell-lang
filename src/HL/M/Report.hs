{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read the Haskell report from file, do necessary transformations
-- to get a reasonable page out of it.

module HL.M.Report where

import           HL.C
import           HL.Types

import           Control.Exception
import qualified Data.ByteString as S
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.List.TakeDrop as CL
import           Data.Monoid
import           Data.Text.ICU.Convert
import           Data.XML.Types
import           System.Directory
import           System.FilePath
import           Text.HTML.DOM
import           Text.XML.Stream.Render

-- | Get a report's page. The Haskell report is ISO-8859-1 encoded
-- rather than UTF-8 so we use ICU to decode it from that instead,
-- strip out surrounding HTML tags, and then return it as normal Html.
getReportPage :: Int -> FilePath -> IO Html
getReportPage year path =
  do exists <- doesFileExist fp
     converter <- open "iso-8859-1" (Just True)
     if exists
        then do !text <- fmap id (S.readFile fp)
                stripWrapper text
        else throw (ReportPageNotFound fp)
  where normalize = filter (\c -> isDigit c || isLetter c || c == '.')
        fp = "static" </> "report" </> ("haskell" ++ show year) </> normalize path

-- | Rather than parsing the HTML, which is slower, we simply strip
-- out any text until we see <body> and keep until we see </body>.
stripWrapper :: S.ByteString -> IO Html
stripWrapper x =
  fmap (preEscapedToMarkup . mconcat) (conduit $$ CL.consume)
  where conduit =
          CL.sourceList [x] $=
          eventConduit $=
          CL.dropWhile (not . matchBegin "body") $=
          CL.takeWhile (not . matchEnd "body") $=
          renderText def

-- | Match a beginning element.
matchBegin :: Name -> Event -> Bool
matchBegin n e =
  case e of { EventBeginElement n' _ -> n == n'; _ -> False }

-- | Match an ending element.
matchEnd :: Name -> Event -> Bool
matchEnd n e =
  case e of { EventEndElement n' -> n == n'; _ -> False }
