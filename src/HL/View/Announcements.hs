{-# LANGUAGE OverloadedStrings #-}
-- |

module HL.View.Announcements where

import HL.Types
import HL.View
import HL.View.Template

announcementsFromMarkdown :: View App () -> View App ()
announcementsFromMarkdown md =
  template
           "Announcements"
           (container_ (row_ (span12_ [class_ "col-md-12"] md)))
