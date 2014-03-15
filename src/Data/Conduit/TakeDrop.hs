-- | Take/drop tools.

module Data.Conduit.List.TakeDrop where

takeWhile :: Monad m => (a -> Bool) -> Conduit a m a
takeWhile p =
  do m <- await
     case m of
       Nothing -> return ()
       Just x | p x -> do yield x
                          takeWhile p
              | otherwise -> return ()


dropWhile :: Monad m => (a -> Bool) -> Conduit a m a
dropWhile p =
  do m <- await
     case m of
       Nothing -> return ()
       Just x | p x -> dropWhile p
              | otherwise -> awaitForever yield
