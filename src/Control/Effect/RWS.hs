{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.RWS
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The effect that combines the reader, writer and state effect, similar to the
-- @MonadRWS@ type class from the @mtl@ library.
--
-- Lazy and strict interpretations of the effect are available here:
-- "Control.Effect.RWS.Lazy" and "Control.Effect.RWS.Strict".
-----------------------------------------------------------------------------
module Control.Effect.RWS
  ( -- * Tagged RWS Effect
    RWS'(..)
    -- * Untagged RWS Effect
    -- | If you don't require disambiguation of multiple RWS effects
    -- (i.e., you only have one RWS effect in your monadic context),
    -- it is recommended to always use the untagged RWS effect.
  , RWS
  , ask
  , local
  , tell
  , listen
  , censor
  , get
  , put
    -- * Convenience Functions
    -- ** Reader Convenience
    -- | If you don't require disambiguation of multiple RWS effects
    -- (i.e., you only have one RWS effect in your monadic context),
    -- it is recommended to always use the untagged functions.
  , asks'
  , asks
    -- ** Writer Convenience
    -- | If you don't require disambiguation of multiple RWS effects
    -- (i.e., you only have one RWS effect in your monadic context),
    -- it is recommended to always use the untagged functions.
  , listens'
  , listens
    -- ** State Convenience
    -- | If you don't require disambiguation of multiple RWS effects
    -- (i.e., you only have one RWS effect in your monadic context),
    -- it is recommended to always use the untagged functions.
  , gets'
  , gets
  , modify'
  , modify
  , modifyStrict'
  , modifyStrict
    -- * Interpretations
  , Separation(..)
  , runSeparatedRWS'
  , runSeparatedRWS
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged RWS effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagRWS'' \@\"newTag\" program
    --     'retagRWS'' \@\"oldTag\" \@\"newTag\" program
    --     'untagRWS'' \@\"erasedTag\" program
    -- @
    -- 
  , tagRWS'
  , retagRWS'
  , untagRWS'
  ) where

-- base
import Data.Coerce (coerce)
import Data.Tuple  (swap)

-- transformers
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.CPS  as Strict

import qualified Control.Effect.Reader as R
import qualified Control.Effect.State  as S
import qualified Control.Effect.Writer as W

import Control.Effect.Machinery

-- | An effect that adds the following features to a given computation:
--
--     * (R) an immutable environment (the \"reader\" part)
--     * (W) a write-only, accumulated output (the \"writer\" part)
--     * (S) a mutable state (the \"state\" part)
class Monad m => RWS' tag r w s m | tag m -> r w s where
  -- | Gets the environment.
  ask' :: m r
  
  -- | Executes a sub-computation in a modified environment.
  local' :: (r -> r) -- ^ The function to modify the environment.
         -> m a      -- ^ The sub-computation to run in the modified environment.
         -> m a      -- ^ The result of the sub-computation.

  -- | Produces the output @w@. In other words, @w@ is appended to the accumulated output.
  tell' :: w -> m ()
  
  -- | Executes a sub-computation and appends @w@ to the accumulated output.
  listen' :: m a -> m (w, a)
  
  -- | Executes a sub-computation and applies the function to its output.
  censor' :: (w -> w) -- ^ The function which is applied to the output.
          -> m a      -- ^ The sub-computation which produces the modified output.
          -> m a      -- ^ The result of the sub-computation.
  
  -- | Gets the current state.
  get' :: m s
  
  -- | Replaces the state with a new value.
  put' :: s -> m ()

makeTaggedEffect ''RWS'

instance (Monad m, Monoid w) => RWS' tag r w s (Lazy.RWST r w s m) where
  ask' = Lazy.ask
  {-# INLINE ask' #-}
  local' = Lazy.local
  {-# INLINE local' #-}
  tell' = Lazy.tell
  {-# INLINE tell' #-}
  listen' = fmap swap . Lazy.listen
  {-# INLINE listen' #-}
  censor' = Lazy.censor
  {-# INLINE censor' #-}
  get' = Lazy.get
  {-# INLINE get' #-}
  put' = Lazy.put
  {-# INLINE put' #-}

instance (Monad m, Monoid w) => RWS' tag r w s (Strict.RWST r w s m) where
  ask' = Strict.ask
  {-# INLINE ask' #-}
  local' = Strict.local
  {-# INLINE local' #-}
  tell' = Strict.tell
  {-# INLINE tell' #-}
  listen' = fmap swap . Strict.listen
  {-# INLINE listen' #-}
  censor' = Strict.censor
  {-# INLINE censor' #-}
  get' = Strict.get
  {-# INLINE get' #-}
  put' = Strict.put
  {-# INLINE put' #-}

-- | Gets a specific component of the environment, using the provided projection function.
asks' :: forall tag r w s m a. RWS' tag r w s m
      => (r -> a) -- ^ The projection function to apply to the environment.
      -> m a      -- ^ The result of the projection.
asks' = flip fmap (ask' @tag)
{-# INLINE asks' #-}

-- | The untagged version of 'asks''.
asks :: RWS r w s m => (r -> a) -> m a
asks = asks' @G
{-# INLINE asks #-}

-- | Executes a sub-computation and applies the function to its output, thus adding
-- an additional value to the result of the sub-computation.
listens' :: forall tag r w s b m a. RWS' tag r w s m
         => (w -> b) -- ^ The function which is applied to the output.
         -> m a      -- ^ The sub-computation which produces the modified output.
         -> m (b, a) -- ^ The result of the sub-computation, including the modified output.
listens' f action = do
  ~(w, a) <- listen' @tag action
  pure (f w, a)
{-# INLINE listens' #-}

-- | The untagged version of 'listens''.
listens :: RWS r w s m => (w -> b) -> m a -> m (b, a)
listens = listens' @G
{-# INLINE listens #-}

-- | Gets a specific component of the state, using the provided projection function.
gets' :: forall tag r w s m a. RWS' tag r w s m => (s -> a) -> m a
gets' f = fmap f (get' @tag)
{-# INLINE gets' #-}

-- | The untagged version of 'gets''.
gets :: RWS r w s m => (s -> a) -> m a
gets f = fmap f get
{-# INLINE gets #-}

-- | Modifies the state, using the provided function.
modify' :: forall tag r w s m. RWS' tag r w s m => (s -> s) -> m ()
modify' f = do
  s <- get' @tag
  put' @tag (f s)
{-# INLINE modify' #-}

-- | The untagged version of 'modify''.
modify :: RWS r w s m => (s -> s) -> m ()
modify = modify' @G
{-# INLINE modify #-}

-- | Modifies the state, using the provided function.
-- The computation is strict in the new state.
modifyStrict' :: forall tag r w s m. RWS' tag r w s m => (s -> s) -> m ()
modifyStrict' f = do
  s <- get' @tag
  put' @tag $! f s
{-# INLINE modifyStrict' #-}

-- | The untagged version of 'modifyStrict''.
modifyStrict :: RWS r w s m => (s -> s) -> m ()
modifyStrict = modifyStrict' @G
{-# INLINE modifyStrict #-}

-- | The separation interpreter of the RWS effect. This type implements the 'RWS''
-- type class by splitting the effect into separate 'R.Reader'', 'W.Writer'' and
-- 'S.State'' effects which can then be interpreted individually.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype Separation m a =
  Separation { runSeparation :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via Default
    deriving (MonadBase b, MonadBaseControl b)

instance (R.Reader' tag r m, W.Writer' tag w m, S.State' tag s m) => RWS' tag r w s (Separation m) where
  ask' = Separation (R.ask' @tag)
  {-# INLINE ask' #-}
  local' f = Separation . R.local' @tag f . runSeparation
  {-# INLINE local' #-}
  tell' = Separation . W.tell' @tag
  {-# INLINE tell' #-}
  listen' = Separation . W.listen' @tag . runSeparation
  {-# INLINE listen' #-}
  censor' f = Separation . W.censor' @tag f . runSeparation
  {-# INLINE censor' #-}
  get' = Separation (S.get' @tag)
  {-# INLINE get' #-}
  put' = Separation . S.put' @tag
  {-# INLINE put' #-}

-- | Runs the RWS effect via separation.
runSeparatedRWS' :: (RWS' tag r w s `Via` Separation) m a -- ^ The program whose RWS effect should be handled.
                 -> m a                                   -- ^ The program with its RWS effect handled.
runSeparatedRWS' = coerce
{-# INLINE runSeparatedRWS' #-}

-- | The untagged version of 'runSeparatedRWS''.
runSeparatedRWS :: (RWS r w s `Via` Separation) m a -> m a
runSeparatedRWS = coerce
{-# INLINE runSeparatedRWS #-}