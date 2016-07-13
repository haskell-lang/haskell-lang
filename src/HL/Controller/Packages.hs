{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GetStarted page controller.

module HL.Controller.Packages
    ( getPackagesR
    , getPackageR
    ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import           HL.Controller
import           HL.View
import           HL.Types.Test
import           HL.View.Test
import           HL.View.Packages
import qualified Language.Haskell.Interpreter as Hint

-- | Packages controller.
getPackagesR :: C (Html ())
getPackagesR = do
    !info <- fmap appPackageInfo getYesod
    result <-
        Hint.runInterpreter
            (do Hint.loadModules
                    [ "src/HL/View/Test.hs"
                    ,"src/HL/Types/Test.hs"]
                ms <- Hint.getLoadedModules
                Hint.setTopLevelModules ms
                packagesV' <-
                    Hint.interpret
                        "packagesV"
                        (Hint.as :: Foo -> Html ())
                let !r = packagesV' (Foo 1 (unMarkdown (piIntro info)))
                return r)
    case result of
        Left e -> lucid (p_ (toHtml (show e)))
        Right html -> lucid (toHtmlRaw (renderBS html))

-- | Package controller.
getPackageR :: PackageName -> C (Html ())
getPackageR name =
  do info <- fmap appPackageInfo getYesod
     case V.find ((== name) . packageName)
                 (piFundamentals info) of
       Nothing ->
         case V.find ((== name) . packageName)
                     (V.concatMap commonChoices
                                  (piCommons info)) of
           Nothing -> redirect PackagesR
           Just package -> handlePackage package
       Just package -> handlePackage package
  where handlePackage package =
          case packagePage package of
            Nothing -> redirect PackagesR
            Just md -> undefined
