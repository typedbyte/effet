{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Error
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The error effect, similar to the @MonadError@ type class from the
-- @mtl@ library.
-----------------------------------------------------------------------------
module Control.Effect.Error
  ( -- * Tagged Error Effect
    Error'(..)
    -- * Untagged Error Effect
    -- | If you don't require disambiguation of multiple error effects
    -- (i.e., you only have one error effect in your monadic context),
    -- it is recommended to always use the untagged error effect.
  , Error
  , throwError
  , catchError
    -- * Convenience Functions
    -- | If you don't require disambiguation of multiple error effects
    -- (i.e., you only have one error effect in your monadic context),
    -- it is recommended to always use the untagged functions.
  , liftEither'
  , liftEither
    -- * Interpretations
  , runError'
  , runError
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged error effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagError'' \@\"newTag\" program
    --     'retagError'' \@\"oldTag\" \@\"newTag\" program
    --     'untagError'' \@\"erasedTag\" program
    -- @
    -- 
  , tagError'
  , retagError'
  , untagError'
  ) where

-- base
import Data.Coerce (coerce)

-- transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT), catchE, throwE)

import Control.Effect.Machinery (G, Tagger(Tagger), Via(Via), makeTaggedEffect)

-- | An effect that equips a computation with the ability to throw and catch
-- exceptions.
class Monad m => Error' tag e m | tag m -> e where
  -- | Throws an exception during the computation and begins exception
  -- processing.
  throwError' :: e -> m a
  -- | Catches an exception in order to handle it and return to normal execution.
  catchError' :: m a -> (e -> m a) -> m a

makeTaggedEffect ''Error'

instance Monad m => Error' tag e (ExceptT e m) where
  throwError' = throwE
  {-# INLINE throwError' #-}
  catchError' = catchE
  {-# INLINE catchError' #-}

-- | Lifts an @'Either' e@ into any @'Error'' e@.
liftEither' :: forall tag e m a. Error' tag e m => Either e a -> m a
liftEither' = either (throwError' @tag) pure
{-# INLINE liftEither' #-}

-- | The untagged version of 'liftEither''.
liftEither :: Error e m => Either e a -> m a
liftEither = liftEither' @G
{-# INLINE liftEither #-}

-- | Runs the error effect by wrapping exceptions in the 'Either' type.
runError' :: (Error' tag e `Via` ExceptT e) m a -> m (Either e a)
runError' = coerce
{-# INLINE runError' #-}

-- | The untagged version of 'runError''.
runError :: (Error e `Via` ExceptT e) m a -> m (Either e a)
runError = coerce
{-# INLINE runError #-}