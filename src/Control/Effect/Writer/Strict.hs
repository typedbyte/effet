-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Writer.Strict
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Strict interpretations of the 'Writer'' effect.
--
-- If you don't require disambiguation of multiple writer effects
-- (i.e., you only have one writer effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.Writer.Strict
  ( -- * Interpreter Type
    WriterT
    -- * Tagged Interpretations
  , execWriter'
  , runWriter'
    -- * Untagged Interpretations
  , execWriter
  , runWriter
  ) where

-- base
import Control.Monad (liftM)
import Data.Coerce   (coerce)
import Data.Tuple    (swap)

-- transformers
import qualified Control.Monad.Trans.Writer.CPS as W

import Control.Effect.Machinery
import Control.Effect.Writer (Writer, Writer')

-- This is necessary until the writer CPS instance land in monad-control.
-- See: https://github.com/basvandijk/monad-control/pull/51
-- | The strict interpreter of the writer effect. This type implements the
-- 'Writer'' type class in a strict manner.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype WriterT w m a =
  WriterT { runWriterT :: W.WriterT w m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans)
    deriving (Writer' tag w)

instance MonadBase b m => MonadBase b (WriterT w m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance (MonadBaseControl b m, Monoid w) => MonadBaseControl b (WriterT w m) where
  type StM (WriterT w m) a = ComposeSt (WriterT w) m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

instance Monoid w => MonadTransControl (WriterT w) where
  type StT (WriterT w) a = (a, w)
  liftWith f = WriterT . W.writerT $
    liftM ( \x -> (x, mempty) )
          ( f $ W.runWriterT . runWriterT )
  {-# INLINABLE liftWith #-}
  restoreT = WriterT . W.writerT
  {-# INLINABLE restoreT #-}

-- | Runs the writer effect and returns the final output.
execWriter' :: forall tag w m a. (Monad m, Monoid w)
            => (Writer' tag w `Via` WriterT w) m a -- ^ The program whose writer effect should be handled.
            -> m w                                 -- ^ The program with its writer effect handled, producing the final output @w@.
execWriter' = W.execWriterT . coerce
{-# INLINE execWriter' #-}

-- | The untagged version of 'execWriter''.
execWriter :: (Monad m, Monoid w) => (Writer w `Via` WriterT w) m a -> m w
execWriter = execWriter' @G
{-# INLINE execWriter #-}

-- | Runs the writer effect and returns both the final output and the result of the interpreted program.
runWriter' :: forall tag w m a. (Functor m, Monoid w)
           => (Writer' tag w `Via` WriterT w) m a -- ^ The program whose writer effect should be handled.
           -> m (w, a)                            -- ^ The program with its writer effect handled, producing the final output @w@ and the result @a@.
runWriter' = fmap swap . W.runWriterT . coerce
{-# INLINE runWriter' #-}

-- | The untagged version of 'runWriter''.
runWriter :: (Functor m, Monoid w) => (Writer w `Via` WriterT w) m a -> m (w, a)
runWriter = runWriter' @G
{-# INLINE runWriter #-}