{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Model library.

module HL.M where

import Control.Applicative

-- | The model monad.
newtype Model a = Model (IO a)
  deriving (Monad,Functor,Applicative)
