{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Reader
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The reader effect, similar to the @MonadReader@ type class from the @mtl@
-- library.
-----------------------------------------------------------------------------
module Control.Effect.Reader
  ( -- * Tagged Reader Effect
    Reader'(..)
    -- * Convenience Functions
  , asks'
    -- * Untagged Reader Effect
    -- | If you don't require disambiguation of multiple reader effects
    -- (i.e., you only have one reader effect in your monadic context),
    -- it is recommended to always use the untagged reader effect.
  , Reader
  , ask
  , local
  , reader
  , asks
    -- * Interpretations
  , runReader'
  , runReader
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged reader effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagReader'' \@\"newTag\" program
    --     'retagReader'' \@\"oldTag\" \@\"newTag\" program
    --     'untagReader'' \@\"erasedTag\" program
    -- @
    -- 
  , tagReader'
  , retagReader'
  , untagReader'
  ) where

-- transformers
import qualified Control.Monad.Trans.Reader   as R
import qualified Control.Monad.Trans.RWS.CPS  as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy

import Control.Effect.Machinery

-- | An effect that adds an immutable state (i.e., an \"environment\") to a given
-- computation. The effect allows to read values from the environment, pass values
-- from function to function, and execute sub-computations in a modified environment.
class Monad m => Reader' tag r m | tag m -> r where
  {-# MINIMAL (ask' | reader'), local' #-}
  
  -- | Gets the environment.
  ask' :: m r
  ask' = reader' @tag id
  {-# INLINE ask' #-}
  
  -- | Executes a sub-computation in a modified environment.
  local' :: (r -> r) -- ^ The function to modify the environment.
         -> m a      -- ^ The sub-computation to run in the modified environment.
         -> m a      -- ^ The result of the sub-computation.
  
  -- | Gets a specific component of the environment, using the provided projection function.
  reader' :: (r -> a) -- ^ The projection function to apply to the environment.
          -> m a      -- ^ The result of the projection.
  reader' f = do
    r <- ask' @tag
    pure (f r)
  {-# INLINE reader' #-}

makeTaggedEffect ''Reader'

instance Monad m => Reader' tag r (R.ReaderT r m) where
  ask' = R.ask
  {-# INLINE ask' #-}
  local' = R.local
  {-# INLINE local' #-}
  reader' = R.reader
  {-# INLINE reader' #-}

instance (Monad m, Monoid w) => Reader' tag r (Lazy.RWST r w s m) where
  ask' = Lazy.ask
  {-# INLINE ask' #-}
  local' = Lazy.local
  {-# INLINE local' #-}
  reader' = Lazy.reader
  {-# INLINE reader' #-}

instance Monad m => Reader' tag r (Strict.RWST r w s m) where
  ask' = Strict.ask
  {-# INLINE ask' #-}
  local' = Strict.local
  {-# INLINE local' #-}
  reader' = Strict.reader
  {-# INLINE reader' #-}

-- | Gets a specific component of the environment, using the provided projection function.
asks' :: forall tag r m a. Reader' tag r m
      => (r -> a) -- ^ The projection function to apply to the environment.
      -> m a      -- ^ The result of the projection.
asks' = reader' @tag
{-# INLINE asks' #-}

makeUntagged ['asks']

-- | Runs the reader effect.
runReader' :: forall tag r m a. r                   -- ^ The initial environment.
           -> (Reader' tag r `Via` R.ReaderT r) m a -- ^ The program whose reader effect should be handled.
           -> m a                                   -- ^ The program with its reader effect handled.
runReader' r = flip R.runReaderT r . runVia
{-# INLINE runReader' #-}

-- | The untagged version of 'runReader''.
makeUntagged ['runReader']