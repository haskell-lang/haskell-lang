module Main where

import Control.Monad (void)
import HL.Controller.Feed (getFeedEntries)
import HL.Model.Packages
import HL.Model.Tutorial
import HL.Main (mkApp)
import Test.Hspec
import SpiderWeb
import Data.String (fromString)
import Network.Wai.Handler.Warp (testWithApplication)
import Yesod.Core (toWaiApp)

main :: IO ()
main = hspec $ do
    it "getPackageInfo succeeds" $ void getPackageInfo
    it "getTutorials succeeds" $ void $ getTutorials True
    it "getFeedEntries succeeds" $ void getFeedEntries

    it "link checking" $
      testWithApplication (mkApp >>= toWaiApp) $ \port ->
        void $ download $ fromString $ "http://localhost:" ++ show port
