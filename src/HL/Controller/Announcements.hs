-- |

module HL.Controller.Announcements where

import           Data.Time
import           HL.Controller
import           HL.Model.Markdown
import           HL.View
import           HL.View.Announcements
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import           Yesod.Feed

-- | Announcements controller.
getAnnouncementsR :: C (Html ())
getAnnouncementsR =
  do feeds <- fmap appFeedEntries getYesod
     lucid (announcementsFromMarkdown
              (mapM_ (\entry ->
                        do h1_ (toHtml (feedEntryTitle entry))
                           p_ (strong_ (toHtml (formatTime defaultTimeLocale "%B %d %Y" (feedEntryUpdated entry))))
                           toHtmlRaw (BlazeUtf8.renderHtml (feedEntryContent entry)))
                     feeds))
