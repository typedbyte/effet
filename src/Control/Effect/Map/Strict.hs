-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Map.Strict
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Strict interpretations of the 'Map'' effect.
--
-- If you don't require disambiguation of multiple map effects
-- (i.e., you only have one map effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.Map.Strict
  ( -- * Interpreter Type
    StrictMap
    -- * Tagged Interpretations
  , runMap'
    -- * Untagged Interpretations
  , runMap
  ) where

-- containers
import qualified Data.Map.Strict as M

-- transformers
import qualified Control.Monad.Trans.State.Strict as S

import Control.Effect.Machinery
import Control.Effect.Map (Map, Map', clear', lookup', update')

-- | The strict interpreter of the map effect. This type implements the
-- 'Map'' type class in a strict manner.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype StrictMap k v m a =
  StrictMap { runStrictMap :: S.StateT (M.Map k v) m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl)
    deriving (MonadBase b, MonadBaseControl b)

instance (Monad m, Ord k) => Map' tag k v (StrictMap k v m) where
  clear' = StrictMap $ S.put M.empty
  {-# INLINE clear' #-}
  lookup' = StrictMap . S.gets . M.lookup
  {-# INLINE lookup' #-}
  update' k mv = StrictMap $ S.modify (M.alter (const mv) k)
  {-# INLINE update' #-}

-- | Runs the map effect, initialized with an empty map.
runMap' :: forall tag k v m a. Monad m
        => (Map' tag k v `Via` StrictMap k v) m a -- ^ The program whose map effect should be handled.
        -> m a                                  -- ^ The program with its map effect handled.
runMap' = flip S.evalStateT M.empty . runStrictMap . runVia
{-# INLINE runMap' #-}

-- | The untagged version of 'runMap''.
runMap :: Monad m => (Map k v `Via` StrictMap k v) m a -> m a
runMap = runMap' @G
{-# INLINE runMap #-}