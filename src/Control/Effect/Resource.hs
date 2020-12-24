{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Resource
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The resource effect allows a computation to allocate resources which are
-- guaranteed to be released after their usage.
-----------------------------------------------------------------------------
module Control.Effect.Resource
  ( -- * Tagged Resource Effect
    Resource'(..)
    -- * Convenience Functions
  , finally'
  , onException'
    -- * Untagged Resource Effect
    -- | If you don't require disambiguation of multiple resource effects
    -- (i.e., you only have one resource effect in your monadic context),
    -- it is recommended to always use the untagged resource effect.
  , Resource
  , bracket
  , bracketOnError
  , finally
  , onException
    -- * Interpretations
  , LowerIO
  , runResourceIO'
  , runResourceIO
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged resource effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagResource'' \@\"newTag\" program
    --     'retagResource'' \@\"oldTag\" \@\"newTag\" program
    --     'untagResource'' \@\"erasedTag\" program
    -- @
    -- 
  , tagResource'
  , retagResource'
  , untagResource'
  ) where

-- base
import qualified Control.Exception as IO
import Data.Coerce (coerce)

import Control.Effect.Machinery

-- | An effect that allows a computation to allocate resources which are
-- guaranteed to be released after their usage.
--
-- @since 0.4.0.0
class MonadIO m => Resource' tag m where
  -- | Acquire a resource, use it, and then release the resource after usage.
  bracket' :: m a        -- ^ The computation which acquires the resource.
           -> (a -> m c) -- ^ The computation which releases the resource.
           -> (a -> m b) -- ^ The computation which uses the resource.
           -> m b        -- ^ The result of the computation which used the resource.

  -- | Like 'bracket'', but only performs the release computation if the usage
  -- computation throws an exception.
  bracketOnError' :: m a        -- ^ The computation which acquires the resource.
                  -> (a -> m c) -- ^ The computation which releases the resource.
                  -> (a -> m b) -- ^ The computation which uses the resource.
                  -> m b        -- ^ The result of the computation which used the resource.

makeTaggedEffect ''Resource'

-- | A simpler version of 'bracket'' where one computation is guaranteed to
-- run after another.
finally' :: forall tag m a b. Resource' tag m
         => m a -- ^ The computation to run.
         -> m b -- ^ The computation to run afterwards, even if the first
                --   computation throws an exception.
         -> m a -- ^ The result of the first computation.
finally' use free =
  bracket' @tag (pure ()) (pure free) (const use)
{-# INLINE finally' #-}

-- | A simpler version of 'bracketOnError'' where one computation is guaranteed
-- to run after another in case the first computation throws an exception.
onException' :: forall tag m a b. Resource' tag m
             => m a -- ^ The computation to run.
             -> m b -- ^ The computation to run afterwards, only if the first
                    --   computation throws an exception.
             -> m a -- ^ The result of the first computation.
onException' use free =
  bracketOnError' @tag (pure ()) (const free) (const use)
{-# INLINE onException' #-}

makeUntagged ['finally', 'onException']

-- | The IO-based interpreter of the resource effect. This type implements the
-- 'Resource'' type class by using 'IO.bracket', thus requiring 'IO' at the bottom
-- of the monad transformer stack.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype LowerIO m a =
  LowerIO { _runLowerIO :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance (MonadBaseControl IO m, MonadIO m) => Resource' tag (LowerIO m) where
  bracket' alloc free use =
    control $ \run ->
      IO.bracket
        ( run alloc )
        ( \a -> run (restoreM a >>= free) )
        ( \a -> run (restoreM a >>= use) )
  {-# INLINABLE bracket' #-}
  bracketOnError' alloc free use =
    control $ \run ->
      IO.bracketOnError
        ( run alloc )
        ( \a -> run (restoreM a >>= free) )
        ( \a -> run (restoreM a >>= use) )
  {-# INLINABLE bracketOnError' #-} 

-- | Runs the resource effect using 'IO.bracket'.
runResourceIO' :: (Resource' tag `Via` LowerIO) m a -> m a
runResourceIO' = coerce
{-# INLINE runResourceIO' #-}

-- | The untagged version of 'runResourceIO''.
makeUntagged ['runResourceIO']