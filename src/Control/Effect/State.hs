{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.State
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The state effect, similar to the @MonadState@ type class from the @mtl@
-- library.
--
-- Lazy and strict interpretations of the effect are available here:
-- "Control.Effect.State.Lazy" and "Control.Effect.State.Strict".
-----------------------------------------------------------------------------
module Control.Effect.State
  ( -- * Tagged State Effect
    State'(..)
    -- * Untagged State Effect
    -- | If you don't require disambiguation of multiple state effects
    -- (i.e., you only have one state effect in your monadic context),
    -- it is recommended to always use the untagged state effect.
  , State
  , get
  , put
  , state
    -- * Convenience Functions
    -- | If you don't require disambiguation of multiple state effects
    -- (i.e., you only have one state effect in your monadic context),
    -- it is recommended to always use the untagged functions.
  , gets'
  , gets
  , modify'
  , modify
  , modifyStrict'
  , modifyStrict
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged state effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagState'' \@\"newTag\" program
    --     'retagState'' \@\"oldTag\" \@\"newTag\" program
    --     'untagState'' \@\"erasedTag\" program
    -- @
    -- 
  , tagState'
  , retagState'
  , untagState'
  ) where

-- base
import Data.Tuple (swap)

-- transformers
import qualified Control.Monad.Trans.RWS.CPS      as Strict
import qualified Control.Monad.Trans.RWS.Lazy     as Lazy
import qualified Control.Monad.Trans.State.Lazy   as L
import qualified Control.Monad.Trans.State.Strict as S

import Control.Effect.Machinery

-- | An effect that adds a mutable state to a given computation.
class Monad m => State' tag s m | tag m -> s where
  {-# MINIMAL get', put' | state' #-}
  
  -- | Gets the current state.
  get' :: m s
  get' = state' @tag (\s -> (s, s))
  {-# INLINE get' #-}
  
  -- | Replaces the state with a new value.
  put' :: s -> m ()
  put' s = state' @tag (\_ -> (s, ()))
  {-# INLINE put' #-}
  
  -- | Updates the state and produces a value based on the current state.
  state' :: (s -> (s, a)) -> m a
  state' f = do
    s <- get' @tag
    let ~(s', a) = f s
    put' @tag s'
    pure a
  {-# INLINE state' #-}

makeTaggedEffect ''State'

instance Monad m => State' tag s (L.StateT s m) where
  get' = L.get
  {-# INLINE get' #-}
  put' = L.put
  {-# INLINE put' #-}
  state' = L.state . fmap swap
  {-# INLINE state' #-}

instance Monad m => State' tag s (S.StateT s m) where
  get' = S.get
  {-# INLINE get' #-}
  put' = S.put
  {-# INLINE put' #-}
  state' = S.state . fmap swap
  {-# INLINE state' #-}

instance (Monad m, Monoid w) => State' tag s (Lazy.RWST r w s m) where
  get' = Lazy.get
  {-# INLINE get' #-}
  put' = Lazy.put
  {-# INLINE put' #-}
  state' = Lazy.state . fmap swap
  {-# INLINE state' #-}

instance Monad m => State' tag s (Strict.RWST r w s m) where
  get' = Strict.get
  {-# INLINE get' #-}
  put' = Strict.put
  {-# INLINE put' #-}
  state' = Strict.state . fmap swap
  {-# INLINE state' #-}

-- | Gets a specific component of the state, using the provided projection function.
gets' :: forall tag s m a. State' tag s m => (s -> a) -> m a
gets' f = fmap f (get' @tag)
{-# INLINE gets' #-}

-- | The untagged version of 'gets''.
gets :: State s m => (s -> a) -> m a
gets = gets' @G
{-# INLINE gets #-}

-- | Modifies the state, using the provided function.
modify' :: forall tag s m. State' tag s m => (s -> s) -> m ()
modify' f = do
  s <- get' @tag
  put' @tag (f s)
{-# INLINE modify' #-}

-- | The untagged version of 'modify''.
modify :: State s m => (s -> s) -> m ()
modify = modify' @G
{-# INLINE modify #-}

-- | Modifies the state, using the provided function.
-- The computation is strict in the new state.
modifyStrict' :: forall tag s m. State' tag s m => (s -> s) -> m ()
modifyStrict' f = do
  s <- get' @tag
  put' @tag $! f s
{-# INLINE modifyStrict' #-}

-- | The untagged version of 'modifyStrict''.
modifyStrict :: State s m => (s -> s) -> m ()
modifyStrict = modifyStrict' @G
{-# INLINE modifyStrict #-}