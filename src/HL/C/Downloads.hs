{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page controller.

module HL.C.Downloads where

import HL.C
import HL.M.Markdown
import HL.V.Downloads
import HL.V

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
