{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page view.

module HL.View.Downloads where

import HL.Types
import HL.View
import HL.View.Template

downloadsFromMarkdown :: Html () -> FromLucid App
downloadsFromMarkdown md =
  template [] "Downloads"
    (\_ -> container_ (row_  (span12_ [class_ "col-md-12"]
                                      (do h1_ (toHtml ("Downloads" :: String))
                                          md))))
