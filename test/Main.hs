module Main where

import Control.Monad (void)
import HL.Controller.Feed (getFeedEntries)
import HL.Model.Packages
import HL.Model.Tutorial
import Test.Hspec

main :: IO ()
main = hspec $ do
    it "getPackageInfo succeeds" $ void getPackageInfo
    it "getTutorials succeeds" $ void $ getTutorials True
    it "getFeedEntries succeeds" $ void getFeedEntries
