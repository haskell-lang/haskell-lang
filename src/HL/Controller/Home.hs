-- | Home page controller.

module HL.Controller.Home where

import HL.Controller
import HL.View
import HL.View.Home
import System.Random

-- | Home controller.
getHomeR :: C (Html ())
getHomeR = do
  snippetInfo <- fmap appSnippetInfo getYesod
  i <- liftIO randomIO
  lucid (homeV snippetInfo {siSeed = i})
