{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Get Haskell news.

module HL.Model.News where

import Lucid

import Data.Text.Lazy.Encoding
import Data.Text.Lazy (toStrict)
import Network.HTTP.Conduit
import Prelude hiding (readFile)

getHaskellNews :: IO (Html ())
getHaskellNews =
  do bytes <- simpleHttp "http://haskellnews.org/grouped?embeddable"
     return (toHtmlRaw (toStrict (decodeUtf8 bytes)))
