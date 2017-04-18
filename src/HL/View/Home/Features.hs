{-# LANGUAGE OverloadedStrings #-}

-- | Features list.

module HL.View.Home.Features where

import HL.View
import HL.View.Code

import Data.Monoid

-- | Features section explains what's notable about Haskell as a
-- language.
features :: View App ()
features =
  div_ [class_ "features"]
       (container_
          (do h1_ "Features"
              row_ (do span6_ [class_ "col-md-6"] statically
                       span6_ [class_ "col-md-6"] purefunc)
              row_ (do span6_ [class_ "col-md-6"] inference
                       span6_ [class_ "col-md-6"] concurrent)
              row_ (do span6_ [class_ "col-md-6"] lazy
                       span6_ [class_ "col-md-6"] packages)))

purefunc :: View App ()
purefunc =
  do h2_ "Purely functional"
     p_ "Haskell has no statements, instructions, or variables. Every value is a constant. \
        \Operations that have side-effects or perform IO are described using a separate IO type \
        \ that distinguishes them from ordinary pure values."
     p_ [class_ "expand"] (a_ "Click to expand")
     div_ [class_ "expandable"] $ do
       p_ (do "The following function takes an integer and returns an integer. "
              "By the type it cannot do any side-effects whatsoever, it cannot\
              \ mutate any of its arguments.")
       haskellPre "square :: Int -> Int\n\
                  \square x = x * x"
       p_ (do "The following string concatenation is okay:")
       haskellPre "\"Hello: \" ++ \"World!\" "
       p_ (do "The following string concatenation is a type error:")
       rejectedHaskellPre "Type error" "\"Name: \" ++ getLine"
       p_ (do "Because "
              code_ "getLine"
              " has type "
              code_ "IO String"
              " and not "
              code_ "String"
              ", like "
              code_ "\"Name: \""
              " is. So by the type system you cannot mix and \
              \match purity with impurity.")

statically :: View App ()
statically =
  do h2_ "Statically typed"
     p_ ""
     p_ [class_ "expand"] (a_ "Click to expand")
     div_ [class_ "expandable"] $ do
       p_ "All Haskell values have a type:"
       haskellPre "char = 'a'    :: Char\n\
                  \int = 123     :: Int\n\
                  \fun = isDigit :: Char -> Bool\n"
       p_ "You have to pass the right type of values to functions, or the compiler\
         \ will reject the program:"
       rejectedHaskellPre "Type error" "isDigit 1"
       p_ "You can decode bytes into text:"
       haskellPre "bytes = Crypto.Hash.SHA1.hash \"hello\" :: ByteString\n\
                  \text = decodeUtf8 bytes               :: Text\n"
       p_ "But you cannot decode Text, which is already a vector \
         \of Unicode points:"
       rejectedHaskellPre "Type error" "doubleDecode = decodeUtf8 (decodeUtf8 bytes)"

concurrent :: View App ()
concurrent =
  do h2_ "Concurrent"
     p_ "Haskell lends itself well to concurrent programming due to its explicit \
       \handling of effects. Its flagship compiler, GHC, comes with a high-\
       \performance parallel garbage collector and light-weight concurrency \
       \library containing a number of useful concurrency primitives and \
       \abstractions."
     p_ [class_ "expand"] (a_ "Click to expand")
     div_ [class_ "expandable"] $ do
       p_ "Easily launch threads and communicate with the standard library:"
       haskellPre "main = do\n\
                  \  done <- newEmptyMVar\n\
                  \  forkIO (do putStrLn \"I'm one thread!\"\n\
                  \             putMVar done \"Done!\")\n\
                  \  second <- forkIO (do delayThread 100000\n\
                  \                       putStrLn \"I'm another thread!\")\n\
                  \  killThread second\n\
                  \  msg <- takeMVar done\n\
                  \  putStrLn msg"
       p_ "Use an asynchronous API for threads:"
       haskellPre "do a1 <- async (getURL url1)\n\
                  \   a2 <- async (getURL url2)\n\
                  \   page1 <- wait a1\n\
                  \   page2 <- wait a2\n\
                  \   ..."
       p_ "Atomic threading with software transactional memory:"
       haskellPre "transfer :: Account -> Account -> Int -> IO ()\n\
                   \transfer from to amount =\n\
                   \  atomically (do deposit to amount\n\
                   \                 withdraw from amount)"
       p_ "Atomic transactions must be repeatable, so arbitrary IO is disabled in \
         \the type system:"
       rejectedHaskellPre "Type error" "main = atomically (putStrLn \"Hello!\")"

inference :: View App ()
inference =
  do h2_ "Type inference"
     p_ "Static types without the pain. Specify the types when you want to, let the \
        \compiler infer the types when you don't."
     p_ [class_ "expand"] (a_ "Click to expand")
     div_ [class_ "expandable"] $ do
       p_ "This example has a type signature for every binding:"
       haskellPre "main :: IO ()\n\
                  \main = do line :: String <- getLine\n\
                  \          print (parseDigit line)\n\
                  \  where parseDigit :: String -> Maybe Int\n\
                  \        parseDigit ((c :: Char) : _) =\n\
                  \          if isDigit c\n\
                  \             then Just (ord c - ord '0')\n\
                  \             else Nothing"
       p_ "But you can just write:"
       haskellPre "main = do line <- getLine\n\
                  \          print (parseDigit line)\n\
                  \  where parseDigit (c : _) =\n\
                  \          if isDigit c\n\
                  \             then Just (ord c - ord '0')\n\
                  \             else Nothing"
       p_ "You can also use inference to avoid wasting time explaining \
         \what you want:"
       haskellPre "do ss <- decode \"[\\\"Hello!\\\",\\\"World!\\\"]\"\n\
                  \   is <- decode \"[1,2,3]\"\n\
                  \   return (zipWith (\\s i -> s ++ \" \" ++ show (i + 5)) ss is)\n\
                  \ => Just [\"Hello! 6\",\"World! 7\"]"
       p_ "Types give a parser specification for free, the following \
         \input is not accepted:"
       haskellPre "do ss <- decode \"[1,2,3]\"\n\
                  \   is <- decode \"[null,null,null]\"\n\
                  \   return (zipWith (\\s i -> s ++ \" \" ++ show (i + 5)) ss is)\n\
                  \ => Nothing"

lazy :: View App ()
lazy =
  do h2_ "Lazy"
     p_ "Values are computed when they are needed, rather than when they are defined in code. \
        \ If a value you defined is not used, it never gets evaluated. This means you can spend \
        \less time thinking about how your program is executed, and more time thinking about what \
        \your program does."
     p_ [class_ "expand"] $ a_ "Click to expand"
     div_ [class_ "expandable"] $ do
       p_ "Define control structures easily:"
       haskellPre "when p m = if p then m else return ()\n\
                  \main = do args <- getArgs\n\
                  \          when (null args)\n\
                  \               (putStrLn \"No args specified!\") "
       p_ "If you notice a repeated expression pattern, like "
       haskellPre "if c then t else False"
       p_ "you can give this a name, like "
       haskellPre "and c t = if c then t else False"
       p_ "and then use it with the same effect as the orginal expression."
       p_ (do "Get code re-use by composing lazy functions. It's quite natural\
              \ to express the "
              code_ "any"
              " function by reusing the "
              code_ "map"
              " and "
              code_ "or"
              " functions:")
       haskellPre "any :: (a -> Bool) -> [a] -> Bool\n\
                  \any p = or . map p"
       p_ (do "Reuse the recursion patterns in "
              code_ "map"; ", "; code_ "filter"; ", "; code_ "foldr"; ", etc.")

packages :: View App ()
packages =
  do h2_ "Packages"
     p_ "Open source contribution to Haskell is very active with a wide range \
        \of packages available on the public package servers."
     p_ [class_ "expand"] (a_ "Click to expand")
     div_ [class_ "expandable"] $ do
       p_ "There are 6,954 packages freely available. Here is a sample of the \
         \most common ones:"
       table_ [class_ "packages"] $
         forM_ (alternating pkgs)
               (\((name1,desc1),(name,desc)) ->
                  tr_ (do td_ (a_ [href_ ("https://hackage.haskell.org/package/" <> name)] $ toHtml name)
                          td_ (toHtml desc)
                          td_ [class_ "rhs"] $ a_ [href_ ("https://hackage.haskell.org/package/" <> name1)] $ toHtml name1
                          td_ [class_ "rhs"] $ toHtml desc1))
  where pkgs :: [(Text,Text)]
        pkgs =
          [("base"             , "Prelude, IO, threads")
          ,("bytestring"       , "Binary data")
          ,("text"             , "Unicode text")
          ,("network"          , "Networking")
          ,("directory"        , "File/directory")
          ,("parsec"           , "Parser library")
          ,("attoparsec"       , "Fast parser")
          ,("hspec"            , "RSpec-like tests")
          ,("persistent"       , "Database ORM")
          ,("monad-logger"     , "Logging")
          ,("tar"              , "Tar archives")
          ,("template-haskell" , "Meta-programming")
          ,("time"             , "Date, time, etc.")
          ,("snap"             , "Web framework")
          ,("yesod"            , "Web framework")
          ,("happstack"        , "Web framework")
          ,("fsnotify"         , "Watch filesystem")
          ,("containers"       , "Maps, graphs, sets")
          ,("unix"             , "UNIX bindings")
          ,("hint"             , "Interpret Haskell")
          ,("OpenGL"           , "OpenGL graphics system")
          ,("SDL"              , "SDL binding")
          ,("pango"            , "Text rendering")
          ,("criterion"        , "Benchmarking")
          ,("statistics"       , "Statistical analysis")
          ,("cairo"            , "Cairo graphics")
          ,("glib"             , "GLib library")
          ,("gtk"              , "Gtk+ library")
          ,("resource-pool"    , "Resource pooling")
          ,("test-framework"   , "Testing framework")
          ,("mwc-random"       , "High-quality randoms")
          ,("conduit"          , "Streaming I/O")
          ,("stm"              , "Atomic threading")
          ,("QuickCheck"       , "Property testing")
          ,("cereal"           , "Binary parsing/printing")
          ,("blaze-html"       , "Markup generation")
          ,("http-client"      , "HTTP client engine")
          ,("xml"              , "XML parser/printer")
          ,("yaml"             , "YAML parser/printer")
          ,("zlib"             , "zlib/gzip/raw")
          ,("binary"           , "Serialization")
          ,("pandoc"           , "Markup conversion")
          ,("zip-archive"      , "Zip compression")
          ,("tls"              , "TLS/SSL")
          ,("text-icu"         , "Text encodings")
          ,("warp"             , "Web server")
          ,("async"            , "Asyn concurrency")
          ,("vector"           , "Vectors")
          ,("scientific"       , "Arbitrary-prec. nums")
          ,("pipes"            , "Streaming IO")
          ,("aeson"            , "JSON parser/printer")
          ,("process"          , "Launch processes")
          ,("syb"              , "Generic prog.")
          ,("dlist"            , "Difflists")]

alternating :: [t] -> [(t, t)]
alternating (x:y:xs) = (x,y) : alternating xs
alternating _ = []
