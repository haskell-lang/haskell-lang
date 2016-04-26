{-# LANGUAGE OverloadedStrings #-}
-- |

module HL.View.Announcements where

import HL.Types
import HL.View
import HL.View.Template

announcementsFromMarkdown :: Html () -> FromLucid App
announcementsFromMarkdown md =
  template [] "Announcements"
    (\_ -> container_ (row_  (span12_ [class_ "col-md-12"]
                                      (do h1_ (toHtml ("Announcements" :: String))
                                          md))))
