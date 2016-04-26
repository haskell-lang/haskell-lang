-- |

module HL.Controller.Announcements where

import HL.Controller
import HL.View.Announcements
import HL.Model.Markdown
import HL.View

-- | Announcements controller.
getAnnouncementsR :: C (Html ())
getAnnouncementsR =
  lucid . announcementsFromMarkdown =<<
  io (getMarkdown "announcements.md")
