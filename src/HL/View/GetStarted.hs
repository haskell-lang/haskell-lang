{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page view.

module HL.View.GetStarted where

import HL.Types
import HL.View
import HL.View.Template

downloadsFromMarkdown :: Html () -> FromLucid App
downloadsFromMarkdown md =
  template [] "Get Started with Haskell"
    (\_ -> container_ (row_  (span12_ [class_ "col-md-12"]
                                      (do h1_ (toHtml ("Get Started with Haskell" :: String))
                                          md))))
