{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Downloads page view.

module HL.View.Downloads where

import HL.Foundation
import HL.View.Template

import Blaze.Prelude
import Blaze.Bootstrap

-- | Downloads view.
downloadsV :: Blaze App
downloadsV =
  template
    [(DownloadsR,"Downloads")]
    (\_ ->
       container
         (row
            (span12
               (do h1 [] "Downloads"
                   p []
                     "The Haskell Platform was a comprehensive, robust development \
                     \environment for programming in Haskell. For new users the \
                     \platform makes it trivial to get up and running with a full \
                     \Haskell development environment. For experienced developers, \
                     \the platform provides a comprehensive, standard base for \
                     \commercial and open source Haskell development that maximises \
                     \interoperability and stability of your code."))))
