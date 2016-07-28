module Main where

import Control.Monad (void)
import HL.Controller.Feed (getFeedEntries)
import HL.Model.Packages
import Test.Hspec

main :: IO ()
main = hspec $ do
    it "getPackageInfo succeeds" $ void $ getPackageInfo True
    it "getFeedEntries succeeds" $ void getFeedEntries
