{-# LANGUAGE OverloadedStrings #-}

-- | Model for videos.

module HL.Model.Videos
  (getHomeVideos)
  where

import Data.Text (Text)

-- | Get videos for the home page.
getHomeVideos :: Monad m => m [(Text, Text, Text)]
getHomeVideos =
  return vids

-- | For now we manually encode them until I have time to think of a
-- better way. This interface will be seamlessly replaceable with a
-- config file or so (as far as the controller and view are
-- concerned).
vids :: [(Text,Text, Text)]
vids =
  [("Functional Reactive Programming for Musical User Interfaces by Paul Hudak","https://vimeo.com/96744621","https://i.vimeocdn.com/video/476988542_150x84.jpg")
  ,("Conquering Hadoop with Haskell and Ozgun Ataman","http://vimeo.com/90189610","https://i.vimeocdn.com/video/469235326_150x84.jpg")
  ,("Using Lenses to Structure State with Nathan Bouscal","http://vimeo.com/90184695","https://i.vimeocdn.com/video/469227196_150x84.jpg")
  ,("GHCJS: Bringing Haskell to the Browser, by Luite Stegeman","http://vimeo.com/80895330","https://i.vimeocdn.com/video/456929997_150x84.jpg")
  ,("Jürgen Cito presents todo-example, a lightweight webapp in Haskell","http://vimeo.com/80863583","https://i.vimeocdn.com/video/456929840_150x84.jpg")
  ,("Abstractions for the Functional Roboticist with Anthony Cowley","http://vimeo.com/77164337","https://i.vimeocdn.com/video/452269027_150x84.jpg")]
