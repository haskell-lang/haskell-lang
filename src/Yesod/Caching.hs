{-# LANGUAGE FlexibleContexts #-}
-- | Support for caching pages.

module Yesod.Caching
  (MonadCaching(..)
  ,caching)
  where

import           Blaze.ByteString.Builder
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$),($=),Flush(..),await,yield,ConduitM)
import           Data.Conduit.Binary (sinkFile)
import           Data.Conduit.Blaze
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           Yesod.Core
import           Yesod.Slug

-- | Monad which contains a cache directory to which things can be
-- read and wrote. The 'withCacheDir' may implement a mutual exclusion
-- on this resource.
class (MonadIO m,MonadBaseControl IO m) => MonadCaching m where
  withCacheDir :: (FilePath -> m a) -> m a

-- | With the given key run the given action, caching any content
-- builders.
caching :: (Slug key,HasContentType content,MonadCaching m)
        => key -> m content -> m TypedContent
caching key handler =
  withCacheDir
    (\dir ->
       do let fp =
                dir </>
                T.unpack (toSlug key)
          exists <- liftIO (doesFileExist fp)
          if exists
             then return (TypedContent (getContentType handler)
                                       (ContentFile fp Nothing))
             else do candidate <-
                       liftM toContent handler
                     invalidated handler fp candidate)

-- | Cache is non-existent or invalidated, time to run the user action
-- and store the result.
invalidated :: (HasContentType content,MonadIO m)
   => m content -> FilePath -> Content -> m TypedContent
invalidated handler fp content =
  case content of
    ContentBuilder builder mlen ->
      do liftIO (L.writeFile
                   fp
                   (maybe id
                          (L.take . fromIntegral)
                          mlen
                          (toLazyByteString builder)))
         continue
    ContentSource src ->
      do liftIO (runResourceT
                   (src $= builderToByteStringFlush $=
                    yieldChunks $$ sinkFile fp))
         continue
    ContentDontEvaluate c -> invalidated handler fp c
    _ -> continue
  where continue =
          return (TypedContent (getContentType handler)
                               content)

-- | Yield all the chunks.
yieldChunks :: ConduitM (Flush o) o (ResourceT IO) ()
yieldChunks =
  do ma <- await
     case ma of
       Nothing -> return ()
       Just Flush -> yieldChunks
       Just (Chunk a) ->
         do yield a
            yieldChunks
