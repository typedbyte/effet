{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Map
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The map effect for modeling a mutable collection of key-value pairs.
--
-- Lazy and strict interpretations of the effect are available here:
-- "Control.Effect.Map.Lazy" and "Control.Effect.Map.Strict".
-----------------------------------------------------------------------------
module Control.Effect.Map
  ( -- * Tagged Map Effect
    Map'(..)
    -- * Untagged Map Effect
    -- | If you don't require disambiguation of multiple map effects
    -- (i.e., you only have one map effect in your monadic context),
    -- it is recommended to always use the untagged map effect.
  , Map
  , clear
  , lookup
  , update
    -- * Convenience Functions
    -- | If you don't require disambiguation of multiple map effects
    -- (i.e., you only have one map effect in your monadic context),
    -- it is recommended to always use the untagged functions.
  , delete'
  , delete
  , exists'
  , exists
  , insert'
  , insert
  , modify'
  , modify
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged map effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagMap'' \@\"newTag\" program
    --     'retagMap'' \@\"oldTag\" \@\"newTag\" program
    --     'untagMap'' \@\"erasedTag\" program
    -- @
    -- 
  , tagMap'
  , retagMap'
  , untagMap'
  ) where

-- base
import Data.Maybe     (isJust)
import Prelude hiding (lookup)

import Control.Effect.Machinery (G, Tagger(Tagger), makeTaggedEffect)

-- | An effect that adds a mutable collection of key-value pairs to a given computation.
class Monad m => Map' tag k v m | tag m -> k v where
  -- | Deletes all key-value pairs from the map.
  clear' :: m ()
  -- | Searches for a value that corresponds to a given key.
  -- Returns 'Nothing' if the key cannot be found.
  lookup' :: k -> m (Maybe v)
  -- | Updates the value that corresponds to a given key.
  -- Passing 'Nothing' as the updated value removes the key-value pair from the map.
  update' :: k -> Maybe v -> m ()

makeTaggedEffect ''Map'

-- | Deletes a key and its corresponding value from the map.
delete' :: forall tag k v m. Map' tag k v m => k -> m ()
delete' k = update' @tag k Nothing
{-# INLINE delete' #-}

-- | The untagged version of 'delete''.
delete :: Map k v m => k -> m ()
delete = delete' @G
{-# INLINE delete #-}

-- | Checks if the map contains a given key.
exists' :: forall tag k v m. Map' tag k v m => k -> m Bool
exists' = fmap isJust . lookup' @tag
{-# INLINE exists' #-}

-- | The untagged version of 'exists''.
exists :: Map k v m => k -> m Bool
exists = exists' @G
{-# INLINE exists #-}

-- | Inserts a new key-value pair into the map. If the key is already present
-- in the map, the associated value is replaced with the new value.
insert' :: forall tag k v m. Map' tag k v m => k -> v -> m ()
insert' k = update' @tag k . Just
{-# INLINE insert' #-}

-- | The untagged version of 'insert''.
insert :: Map k v m => k -> v -> m ()
insert = insert' @G
{-# INLINE insert #-}

-- | Updates the value that corresponds to a given key.
-- If the key cannot be found, a corresponding default value is assumed.
modify' :: forall tag k v m. Map' tag k v m
        => v        -- ^ The default value that is assumed if the key is missing.
        -> (v -> v) -- ^ The function for updating the value. This function is
                    -- also applied to the default value if the key is missing.
        -> k        -- ^ The key whose corresponding value is updated.
        -> m ()     -- ^ The operation produces no value.
modify' fallback f k = do
  maybeVal <- lookup' @tag k
  case maybeVal of
    Just v  -> insert' @tag k (f v)
    Nothing -> insert' @tag k (f fallback)
{-# INLINE modify' #-}

-- | The untagged version of 'modify''.
modify :: Map k v m => v -> (v -> v) -> k -> m ()
modify = modify' @G
{-# INLINE modify #-}