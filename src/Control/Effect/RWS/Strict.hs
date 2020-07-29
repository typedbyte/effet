-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.RWS.Strict
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Strict interpretations of the 'RWS'' effect.
--
-- If you don't require disambiguation of multiple RWS effects
-- (i.e., you only have one RWS effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.RWS.Strict
  ( -- * Interpreter Type
    RWST
    -- * Tagged Interpretations
  , evalRWS'
  , execRWS'
  , runRWS'
    -- * Untagged Interpretations
  , evalRWS
  , execRWS
  , runRWS
  ) where

-- base
import Control.Monad (liftM)
import Data.Coerce   (coerce)

-- transformers
import qualified Control.Monad.Trans.RWS.CPS as RWS

import Control.Effect.Machinery
import Control.Effect.Reader    (Reader, Reader')
import Control.Effect.RWS       (RWS, RWS')
import Control.Effect.State     (State, State')
import Control.Effect.Writer    (Writer, Writer')

-- This is necessary until the writer CPS instances land in monad-control.
-- See: https://github.com/basvandijk/monad-control/pull/51
-- | The strict interpreter of the RWS effect. This type implements the
-- 'RWS'' type class in a strict manner.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype RWST r w s m a =
  RWST { runRWST :: RWS.RWST r w s m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans)
    deriving (RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s)

instance MonadBase b m => MonadBase b (RWST r w s m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance (MonadBaseControl b m, Monoid w) => MonadBaseControl b (RWST r w s m) where
  type StM (RWST r w s m) a = ComposeSt (RWST r w s) m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

instance Monoid w => MonadTransControl (RWST r w s) where
  type StT (RWST r w s) a = (a, s, w)
  liftWith f = RWST . RWS.rwsT $
    \r s -> liftM ( \x -> (x, s, mempty) )
                  ( f $ \t -> (RWS.runRWST . runRWST) t r s )
  {-# INLINABLE liftWith #-}
  restoreT mSt = RWST . RWS.rwsT $ \_ _ -> mSt
  {-# INLINABLE restoreT #-}

-- | Runs the RWS effect and discards the final state.
evalRWS'
  :: forall tag r w s m a. (Functor m, Monoid w)
  => r
  -- ^ The initial environment.
  -> s
  -- ^ The initial state.
  -> ('[RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s] `EachVia` RWST r w s) m a
  -- ^ The program whose RWS effect should be handled.
  -> m (w, a)
  -- ^ The program with its RWS effect handled, producing the final
  -- output @w@ and the result @a@.
evalRWS' r s = fmap reorder . (\m -> RWS.runRWST m r s) . coerce
  where
    reorder (a, _, w) = (w, a)
{-# INLINE evalRWS' #-}

-- | The untagged version of 'evalRWS''.
evalRWS :: (Functor m, Monoid w) => r -> s -> ('[RWS r w s, Reader r, Writer w, State s] `EachVia` RWST r w s) m a -> m (w, a)
evalRWS = evalRWS' @G
{-# INLINE evalRWS #-}

-- | Runs the RWS effect and discards the result of the interpreted program.
execRWS'
  :: forall tag r w s m a. (Functor m, Monoid w)
  => r
  -- ^ The initial environment.
  -> s
  -- ^ The initial state.
  -> ('[RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s] `EachVia` RWST r w s) m a
  -- ^ The program whose RWS effect should be handled.
  -> m (w, s)
  -- ^ The program with its RWS effect handled, producing the final
  -- output @w@ and the final state @s@.
execRWS' r s = fmap reorder . (\m -> RWS.runRWST m r s) . coerce
  where
    reorder (_, s', w) = (w, s')
{-# INLINE execRWS' #-}

-- | The untagged version of 'execRWS''.
execRWS :: (Functor m, Monoid w) => r -> s -> ('[RWS r w s, Reader r, Writer w, State s] `EachVia` RWST r w s) m a -> m (w, s)
execRWS = execRWS' @G
{-# INLINE execRWS #-}

-- | Runs the RWS effect and returns the final output, the final state and the result of the interpreted program.
runRWS'
  :: forall tag r w s m a. (Functor m, Monoid w)
  => r
  -- ^ The initial environment.
  -> s
  -- ^ The initial state.
  -> ('[RWS' tag r w s, Reader' tag r, Writer' tag w, State' tag s] `EachVia` RWST r w s) m a
  -- ^ The program whose RWS effect should be handled.
  -> m (w, s, a)
  -- ^ The program with its RWS effect handled, producing the final
  -- output @w@, the final state @s@ and the result @a@.
runRWS' r s = fmap reorder . (\m -> RWS.runRWST m r s) . coerce
  where
    reorder (a, s', w) = (w, s', a)
{-# INLINE runRWS' #-}

-- | The untagged version of 'runRWS''.
runRWS :: (Functor m, Monoid w) => r -> s -> ('[RWS r w s, Reader r, Writer w, State s] `EachVia` RWST r w s) m a -> m (w, s, a)
runRWS = runRWS' @G
{-# INLINE runRWS #-}