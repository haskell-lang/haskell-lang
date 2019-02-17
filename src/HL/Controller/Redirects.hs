{-# LANGUAGE OverloadedStrings #-}
module HL.Controller.Redirects where

import HL.Controller

redirectT :: Text -> C a
redirectT = redirect

getHomeR :: C ()
getHomeR = redirectT "https://haskell.fpcomplete.com/"

-- FIXME getAnnouncementsR
-- FIXME getAnnouncementR

getGetStartedR :: C ()
getGetStartedR = redirectT "https://haskell.fpcomplete.com/get-started"

getGetStartedOSR :: OS -> C ()
getGetStartedOSR os = redirectT $ "https://haskell.fpcomplete.com/get-started/" <> toPathPiece os

getCommunityR :: C ()
getCommunityR = redirectT "https://haskell.fpcomplete.com/community"

getIrcR :: C ()
getIrcR = redirectT "https://haskell.fpcomplete.com/community"

getMailingListsR :: C ()
getMailingListsR = redirectT "https://haskell.fpcomplete.com/community"

getSuccessStoriesR :: C ()
getSuccessStoriesR = redirectT "https://haskell.fpcomplete.com/promote"

getDocumentationR :: C ()
getDocumentationR = redirectT "https://haskell.fpcomplete.com/learn"

getLibrariesR :: C ()
getLibrariesR = redirectT "https://haskell.fpcomplete.com/tutorial/libraries"

getLibrariesSingularR :: C ()
getLibrariesSingularR = redirectT "https://haskell.fpcomplete.com/tutorial/libraries"

getLibraryR :: PackageName -> C ()
getLibraryR name = redirectT $ "https://haskell.fpcomplete.com/library/" <> toPathPiece name

-- FIXME FeedR

-- FIXME InteroR

getTutorialR :: Text -> C ()
getTutorialR name = redirectT $ "https://haskell.fpcomplete.com/tutorial/" <> name
