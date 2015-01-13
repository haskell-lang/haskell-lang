{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page controller.

module HL.Controller.Downloads where

import HL.Controller
import HL.Model.Markdown
import HL.View.Downloads
import HL.View

-- | Downloads controller.
getDownloadsR :: C (Html ())
getDownloadsR = lucid downloadsV

-- | Downloads for particular OS.
getDownloadsForR :: OS -> C (Html ())
getDownloadsForR os =
  do manualInstall <- io (getMarkdown "manual-install.md")
     autoInstall <- io (getMarkdown autoFp)
     lucid (downloadsForV os autoInstall manualInstall)
  where autoFp =
          case os of
            Windows -> "windows-install.md"
            OSX -> "osx-install.md"
            Linux -> "linux-install.md"
