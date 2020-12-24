{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Managed
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The managed effect allows a computation to allocate resources which are
-- guaranteed to be released after the end of the computation. This effect
-- provides a monadic interface for managing one or more long-living
-- resources in a more readable way than nesting 'IO.bracket'-style
-- operations of the "Control.Effect.Resource" effect.
-----------------------------------------------------------------------------
module Control.Effect.Managed
  ( -- * Tagged Managed Effect
    Managed'(..)
    -- * Untagged Managed Effect
    -- | If you don't require disambiguation of multiple managed effects
    -- (i.e., you only have one managed effect in your monadic context),
    -- it is recommended to always use the untagged managed effect.
  , Managed
  , manage
    -- * Interpretations
  , Bracket
  , runManaged'
  , runManaged
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged managed effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagManaged'' \@\"newTag\" program
    --     'retagManaged'' \@\"oldTag\" \@\"newTag\" program
    --     'untagManaged'' \@\"erasedTag\" program
    -- @
    -- 
  , tagManaged'
  , retagManaged'
  , untagManaged'
  ) where

-- base
import qualified Control.Exception as IO
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- transformers
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)

import Control.Effect.Machinery

-- | An effect that allows a computation to allocate resources which are
-- guaranteed to be released after the computation.
--
-- @since 0.4.0.0
class MonadIO m => Managed' tag m where
  -- | Acquire a resource by specifying an acquisition action and a release
  -- action to be used for cleanup after the computation.
  --
  -- @since 0.4.0.0
  manage' :: m a        -- ^ The computation which acquires the resource.
          -> (a -> m b) -- ^ The computation which releases the resource.
          -> m a        -- ^ The acquired resource.

makeTaggedEffect ''Managed'

-- | The bracket-based interpreter of the managed effect. This type implements
-- the 'Managed'' type class by using 'IO.bracket', thus requiring 'IO' at the
-- bottom of the monad transformer stack.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
--
-- @since 0.4.0.0
newtype Bracket n m a = Bracket { runBracket :: ReaderT (IORef [n ()]) m a }
  deriving (Applicative, Functor, Monad, MonadIO)
  deriving (MonadTrans, MonadTransControl)
  deriving (MonadBase b, MonadBaseControl b)

instance (MonadBase IO m, MonadIO m) => Managed' tag (Bracket m m) where
  manage' alloc free = Bracket . ReaderT $
    \ref -> do
      a <- runReaderT (runBracket alloc) ref
      liftBase $
        atomicModifyIORef' ref $
          \frees -> (runReaderT (runBracket (free a >> pure ())) ref : frees, ())
      pure a
  {-# INLINE manage' #-}

-- | Runs the managed effect using 'IO.bracket'.
--
-- @since 0.4.0.0
runManaged' :: forall tag m a. MonadBaseControl IO m => (Managed' tag `Via` Bracket m) m a -> m a
runManaged' program =
  liftedBracket
    ( allocRef )
    ( freeRef  )
    ( runReaderT (runBracket (runVia program)) )
  where
    allocRef :: forall n. MonadBase IO n => n (IORef [n ()])
    allocRef = liftBase $ newIORef []
    
    freeRef :: forall n. MonadBase IO n => IORef [n ()] -> n ()
    freeRef = (sequence_ =<<) . liftBase . readIORef
    
    liftedBracket :: forall n b c d. MonadBaseControl IO n => n b -> (b -> n c) -> (b -> n d) -> n d
    liftedBracket alloc free use =
      control $ \run ->
        IO.bracket
          ( run alloc )
          ( \a -> run (restoreM a >>= free) )
          ( \a -> run (restoreM a >>= use) )
{-# INLINE runManaged' #-}

-- | The untagged version of 'runManaged''.
--
-- @since 0.4.0.0
makeUntagged ['runManaged']