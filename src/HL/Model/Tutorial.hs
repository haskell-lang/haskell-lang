{-# LANGUAGE OverloadedStrings #-}
module HL.Model.Tutorial
    ( getTutorials
    ) where

import Control.Exception.Safe (tryAny, throwIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Traversable (forM)
import HL.Types
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeBaseName, takeExtension, takeFileName)

getTutorials :: Bool -- ^ fail on errors?
             -> IO (Map TutorialKey Tutorial)
getTutorials fatal = do
    files <-
        getDirectoryContents root >>=
        filterM doesFileExist .
        map (root </>)
    maps <- mapM (processFile fatal) files
    let m = Map.unionsWith (++) (map (fmap return) maps)
    fmap Map.fromList $ forM (Map.toList m) $ \(key, vals) ->
        case vals of
            [] -> error $ "getTutorials: impossible empty list"
            [x] -> return (key, x)
            x:_
                | fatal -> error $ "Multiple tutorials for key: " ++ show key
                | otherwise -> do
                    putStrLn $ "Warning: multiple tutorials for key: " ++ show key
                    return (key, x)
  where
    root = "static/tutorial"
    filterM _ [] = return []
    filterM f (x:xs) = do
        res <- f x
        if res
            then fmap (x :) (filterM f xs)
            else filterM f xs

processFile :: Bool -> FilePath -> IO (Map TutorialKey Tutorial)
processFile fatal fp = do
    bsOrig <- S.readFile fp
    let textOrig = decodeUtf8With lenientDecode bsOrig
    mtext <-
        case takeExtension fp of
            ".md" -> return (Just (textOrig, Just (T.pack (takeFileName fp))))
            ".url" -> do
                req <- parseRequest (T.unpack (T.takeWhile (/= '\n') textOrig))
                eres <- tryAny (httpLBS req)
                case eres of
                    Left e
                        | fatal -> throwIO e
                        | otherwise -> do
                            putStrLn $ "Error occurred downloading tutorial: " ++ show e
                            return Nothing
                    Right res ->
                        let lbs = getResponseBody res
                            text = stripHeader (decodeUtf8With lenientDecode (L.toStrict lbs))
                        in return (Just (text, Nothing))
            _
                | "#" `T.isSuffixOf` T.pack fp -> return Nothing
                | "~" `T.isSuffixOf` T.pack fp -> return Nothing
                | fatal -> error $ "Invalid file extension for tutorial: " ++ fp
                | otherwise -> do
                    putStrLn $ "Invalid file extension for tutorial: " ++ fp
                    return Nothing

    case mtext of
        Nothing -> return Map.empty
        Just (text, mlocalFilename) -> do
            let base = T.pack (takeBaseName fp)
                (tutorialKey, title, markdown) =
                    case T.stripPrefix "package-" base of
                        Nothing -> (RegularTutorial base, getTitleSimple text, text)
                        Just pkgName ->
                          ( PackageTutorial (PackageName pkgName)
                          , pkgName <> " library"
                          , T.concat
                              [ "# "
                              , pkgName
                              , " library\n\n"
                              , stripExistingTitle text
                              ]
                          )
            return $ Map.singleton tutorialKey Tutorial
                { tutorialTitle = title
                , tutorialContent = Markdown markdown
                , tutorialLocalFilename = mlocalFilename
                }

-- | A simple approach to getting the title from a Markdown file
getTitleSimple :: Text -> Text
getTitleSimple = T.strip . T.takeWhile (/= '\n') . T.dropWhile (== '#')

-- | If present, strip a --- header from a Markdown file.
stripHeader :: T.Text -> T.Text
stripHeader t0 = fromMaybe t0 $ do
    t1 <- T.stripPrefix "---\n" t0
    let ls = T.lines t1
    case dropWhile (/= "---") ls of
        [] -> Nothing
        (_:rest) -> Just (T.unlines rest)

-- | Remove the first heading found in the content
stripExistingTitle :: T.Text -> T.Text
stripExistingTitle t0
    | T.null t0 || T.head t0 /= '#' = t0
    | otherwise = T.dropWhile (/= '\n') t0
