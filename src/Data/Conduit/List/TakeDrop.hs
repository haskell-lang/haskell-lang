-- | Take/drop tools.

module Data.Conduit.List.TakeDrop where

import Data.Conduit
import Prelude hiding (takeWhile,dropWhile)

-- | Take elements and yield them into upstream while the given
-- predicate holds, afterwards stop consuming.
takeWhile :: Monad m => (a -> Bool) -> Conduit a m a
takeWhile p =
  do m <- await
     case m of
       Nothing -> return ()
       Just x | p x -> do yield x
                          takeWhile p
              | otherwise -> return ()

-- | Consume elements and discard them until a given predicate holds,
-- then yield everything following from upstream to downstream.
dropWhile :: Monad m => (a -> Bool) -> Conduit a m a
dropWhile p =
  do m <- await
     case m of
       Nothing -> return ()
       Just x | p x -> dropWhile p
              | otherwise -> awaitForever yield
