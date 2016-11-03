{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getLibrariesR
    , getLibrariesSingularR
    , getLibraryR
    ) where

import           HL.Controller
import           HL.Controller.Tutorial (displayTutorial)
import           HL.View
import           HL.View.Packages
import           Network.HTTP.Types (status301)

-- | Packages controller.
getLibrariesR :: C (Html ())
getLibrariesR =
  do info <- fmap appPackageInfo getYesod
     lucid (packagesV info)

-- | Synonym to avoid 404s, see:
-- https://www.reddit.com/r/haskell/comments/57aauu/new_conduit_tutorial/d8rszhy
getLibrariesSingularR :: C ()
getLibrariesSingularR = redirectWith status301 LibrariesR

-- | Package controller.
getLibraryR :: PackageName -> C (Html ())
getLibraryR = displayTutorial . PackageTutorial
