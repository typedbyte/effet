-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Map.Lazy
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Lazy interpretations of the 'Map'' effect.
--
-- If you don't require disambiguation of multiple map effects
-- (i.e., you only have one map effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.Map.Lazy
  ( -- * Interpreter Implementation
    LazyMap
  , clear
  , lookup
  , update
    -- * Tagged Interpretations
  , runMap'
    -- * Untagged Interpretations
  , runMap
  ) where

-- base
import Prelude hiding (lookup)

-- containers
import qualified Data.Map.Lazy as M

-- transformers
import qualified Control.Monad.Trans.State.Lazy as S

import Control.Effect.Machinery
import Control.Effect.Map (Map, Map', clear', lookup', update')

-- | The lazy interpreter of the map effect. This type implements the
-- 'Map'' type class in a lazy manner.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype LazyMap k v m a =
  LazyMap { runLazyMap :: S.StateT (M.Map k v) m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl)
    deriving (MonadBase b, MonadBaseControl b)

instance (Monad m, Ord k) => Map' tag k v (LazyMap k v m) where
  clear' = clear
  {-# INLINE clear' #-}
  lookup' = lookup
  {-# INLINE lookup' #-}
  update' = update
  {-# INLINE update' #-}

-- | Deletes all key-value pairs from the map.
clear :: Monad m => LazyMap k v m ()
clear = LazyMap $ S.put M.empty
{-# INLINE clear #-}

-- | Searches for a value that corresponds to a given key.
-- Returns 'Nothing' if the key cannot be found.
lookup :: (Monad m, Ord k) => k -> LazyMap k v m (Maybe v)
lookup = LazyMap . S.gets . M.lookup
{-# INLINE lookup #-}

-- | Updates the value that corresponds to a given key.
-- Passing 'Nothing' as the updated value removes the key-value pair from the map.
update :: (Monad m, Ord k) => k -> Maybe v -> LazyMap k v m ()
update k mv = LazyMap $ S.modify (M.alter (const mv) k)
{-# INLINE update #-}

-- | Runs the map effect, initialized with an empty map.
runMap' :: forall tag k v m a. Monad m
        => (Map' tag k v `Via` LazyMap k v) m a -- ^ The program whose map effect should be handled.
        -> m a                                  -- ^ The program with its map effect handled.
runMap' = flip S.evalStateT M.empty . runLazyMap . runVia
{-# INLINE runMap' #-}

-- | The untagged version of 'runMap''.
runMap :: Monad m => (Map k v `Via` LazyMap k v) m a -> m a
runMap = runMap' @G
{-# INLINE runMap #-}