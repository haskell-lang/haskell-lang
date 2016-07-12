{-# LANGUAGE BangPatterns #-}

-- | Get html templates.

module HL.Controller.Html where

import qualified Data.Text.IO as T
import           HL.Controller
import           HL.View
import           HL.View.Template

-- | Render a simple html page.
htmlPage :: Text -> FilePath -> C (Html ())
htmlPage t name =
  do dir <- getStaticDir
     content <- io (T.readFile (dir ++ "/html/" ++ name))
     lucid (template  t (toHtmlRaw content))
