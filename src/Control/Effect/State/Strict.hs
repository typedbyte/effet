-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.State.Strict
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Strict interpretations of the 'State'' effect.
--
-- If you don't require disambiguation of multiple state effects
-- (i.e., you only have one state effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.State.Strict
  ( -- * Tagged Interpretations
    evalState'
  , execState'
  , runState'
    -- * Untagged Interpretations
  , evalState
  , execState
  , runState
  ) where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.State.Strict (StateT, runStateT)

import Control.Effect.State     (State, State')
import Control.Effect.Machinery (G, Via, runVia)

-- | Runs the state effect and discards the final state.
evalState' :: forall tag s m a. Functor m
           => s                                 -- ^ The initial state.
           -> (State' tag s `Via` StateT s) m a -- ^ The program whose state effect should be handled.
           -> m a                               -- ^ The program with its state effect handled.
evalState' s = fmap fst . flip runStateT s . runVia
{-# INLINE evalState' #-}

-- | The untagged version of 'evalState''.
evalState :: Functor m => s -> (State s `Via` StateT s) m a -> m a
evalState = evalState' @G
{-# INLINE evalState #-}

-- | Runs the state effect and returns the final state.
execState' :: forall tag s m a. Functor m
           => s                                 -- ^ The initial state.
           -> (State' tag s `Via` StateT s) m a -- ^ The program whose state effect should be handled.
           -> m s                               -- ^ The program with its state effect handled, producing the final state @s@.
execState' s = fmap snd . flip runStateT s . runVia
{-# INLINE execState' #-}

-- | The untagged version of 'execState''.
execState :: Functor m => s -> (State s `Via` StateT s) m a -> m s
execState = execState' @G
{-# INLINE execState #-}

-- | Runs the state effect and returns both the final state and the result of the interpreted program.
runState' :: forall tag s m a. Functor m
          => s                                 -- ^ The initial state.
          -> (State' tag s `Via` StateT s) m a -- ^ The program whose state effect should be handled.
          -> m (s, a)                          -- ^ The program with its state effect handled, producing the final state @s@ and the result @a@.
runState' s = fmap swap . flip runStateT s . runVia
{-# INLINE runState' #-}

-- | The untagged version of 'runState''.
runState :: Functor m => s -> (State s `Via` StateT s) m a -> m (s, a)
runState = runState' @G
{-# INLINE runState #-}