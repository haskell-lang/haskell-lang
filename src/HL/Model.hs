{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Model library.

module HL.Model where

import Control.Applicative

-- | The model monad.
newtype Model a = Model (IO a)
  deriving (Monad,Functor,Applicative)
