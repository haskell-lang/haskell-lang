-- | Devel web server.

module DevelMain where

import HL.Foundation
import HL.Dispatch ()

import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp
import Yesod.Static

-- | Start the web server.
main :: IO (Store (IORef Application))
main =
  do s <- static "static"
     c <- newChan
     app <- toWaiApp (App s c)
     ref <- newIORef app
     tid <- forkIO
              (runSettings
                 (defaultSettings { settingsPort = 1990 })
                 (\req ->
                    do handler <- readIORef ref
                       handler req))
     _ <- newStore tid
     ref <- newStore ref
     newStore c
     return ref

-- | Update the server, start it if not running.
update :: IO (Store (IORef Application))
update =
  do m <- lookupStore 1
     case m of
       Nothing -> main
       Just store ->
         do ref <- readStore store
            c <- readStore (Store 2)
            writeChan c ()
            s <- static "static"
            app <- toWaiApp (App s c)
            writeIORef ref app
            return store
