-- | Home page controller.

module HL.C.Home where

import HL.C
import HL.M.Videos
import HL.V
import HL.V.Home

-- | Home controller.
getHomeR :: C (Html ())
getHomeR =
  do vids <- getHomeVideos
     lucid (homeV vids)
