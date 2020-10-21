-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Identity
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides the function 'runIdentity' for extracting the final
-- result of pure effect interpretations.
-----------------------------------------------------------------------------
module Control.Effect.Identity
  ( -- * Interpretations
    runIdentity
  ) where

-- base
import qualified Data.Functor.Identity as I

-- | Runs a computation using the 'I.Identity' monad.
--
-- You usually need this when an expression of type @Monad m => m a@ remains
-- after handling all the effects and you want to extract its pure result.
runIdentity :: I.Identity a -> a
runIdentity = I.runIdentity
{-# INLINE runIdentity #-}