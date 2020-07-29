-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.RWS.Lazy
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Lazy interpretations of the 'RWS'' effect.
--
-- If you don't require disambiguation of multiple RWS effects
-- (i.e., you only have one RWS effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.RWS.Lazy
  ( -- * Tagged Interpretations
    evalRWS'
  , execRWS'
  , runRWS'
    -- * Untagged Interpretations
  , evalRWS
  , execRWS
  , runRWS
  ) where

-- transformers
import Control.Monad.Trans.RWS.Lazy (RWST, runRWST)

import Control.Effect.Machinery (EachVia, G, runVia)
import Control.Effect.Reader    (Reader, Reader')
import Control.Effect.RWS       (RWS, RWS')
import Control.Effect.State     (State, State')
import Control.Effect.Writer    (Writer, Writer')

-- | Runs the RWS effect and discards the final state.
evalRWS'
  :: forall tag r w s m a. Functor m
  => r
  -- ^ The initial environment.
  -> s
  -- ^ The initial state.
  -> ('[RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s] `EachVia` RWST r w s) m a
  -- ^ The program whose RWS effect should be handled.
  -> m (w, a)
  -- ^ The program with its RWS effect handled, producing the final
  -- output @w@ and the result @a@.
evalRWS' r s = fmap reorder . (\m -> runRWST m r s) . runVia
  where
    reorder (a, _, w) = (w, a)
{-# INLINE evalRWS' #-}

-- | The untagged version of 'evalRWS''.
evalRWS :: Functor m => r -> s -> ('[RWS r w s, Reader r, Writer w, State s] `EachVia` RWST r w s) m a -> m (w, a)
evalRWS = evalRWS' @G
{-# INLINE evalRWS #-}

-- | Runs the RWS effect and discards the result of the interpreted program.
execRWS'
  :: forall tag r w s m a. Functor m
  => r
  -- ^ The initial environment.
  -> s
  -- ^ The initial state.
  -> ('[RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s] `EachVia` RWST r w s) m a
  -- ^ The program whose RWS effect should be handled.
  -> m (w, s)
  -- ^ The program with its RWS effect handled, producing the final
  -- output @w@ and the final state @s@.
execRWS' r s = fmap reorder . (\m -> runRWST m r s) . runVia
  where
    reorder (_, s', w) = (w, s')
{-# INLINE execRWS' #-}

-- | The untagged version of 'execRWS''.
execRWS :: Functor m => r -> s -> ('[RWS r w s, Reader r, Writer w, State s] `EachVia` RWST r w s) m a -> m (w, s)
execRWS = execRWS' @G
{-# INLINE execRWS #-}

-- | Runs the RWS effect and returns the final output, the final state and the
-- result of the interpreted program.
runRWS'
  :: forall tag r w s m a. Functor m
  => r
  -- ^ The initial environment.
  -> s
  -- ^ The initial state.
  -> ('[RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s] `EachVia` RWST r w s) m a
  -- ^ The program whose RWS effect should be handled.
  -> m (w, s, a)
  -- ^ The program with its RWS effect handled, producing the final
  -- output @w@, the final state @s@ and the result @a@.
runRWS' r s = fmap reorder . (\m -> runRWST m r s) . runVia
  where
    reorder (a, s', w) = (w, s', a)
{-# INLINE runRWS' #-}

-- | The untagged version of 'runRWS''.
runRWS :: Functor m => r -> s -> ('[RWS r w s, Reader r, Writer w, State s] `EachVia` RWST r w s) m a -> m (w, s, a)
runRWS = runRWS' @G
{-# INLINE runRWS #-}