{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Embed
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The embed effect for integrating arbitrary monads into the effect system.
-----------------------------------------------------------------------------
module Control.Effect.Embed
  ( -- * Tagged Embed Effect
    Embed'(..)
    -- * Untagged Embed Effect
    -- | If you don't require disambiguation of multiple embed effects
    -- (i.e., you only have one embed effect in your monadic context),
    -- it is recommended to always use the untagged embed effect.
  , Embed
  , embed
    -- * Interpretations
    -- ** Via Transformation
  , Transformation
  , runEmbed'
  , runEmbed
    -- ** Via Finalization
  , Finalization
  , runFinal'
  , runFinal
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged embed effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagEmbed'' \@\"newTag\" program
    --     'retagEmbed'' \@\"oldTag\" \@\"newTag\" program
    --     'untagEmbed'' \@\"erasedTag\" program
    -- @
    -- 
  , tagEmbed'
  , retagEmbed'
  , untagEmbed'
  ) where

-- base
import Data.Coerce           (coerce)
import Data.Functor.Identity (Identity)

-- transformers
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)

import Control.Effect.Machinery hiding (embed)

-- | An effect that integrates a monad @n@ into the computation @m@.
--
-- @since 0.3.0.0
class Monad m => Embed' tag n m | tag m -> n where
  -- | Monadic actions in @n@ can be lifted into @m@ via 'embed'.
  --
  -- 'embed' is like 'liftIO', but not limited to 'IO'. In fact, 'liftIO' can
  -- be realized using 'embed' by specializing @n@ to @IO@.
  --
  -- @since 0.3.0.0
  embed' :: n a -> m a

makeTaggedEffect ''Embed'

instance Embed' tag IO IO where
  embed' = id
  {-# INLINE embed' #-}

instance Embed' tag Maybe Maybe where
  embed' = id
  {-# INLINE embed' #-}

instance Embed' tag [] [] where
  embed' = id
  {-# INLINE embed' #-}

instance Embed' tag Identity Identity where
  embed' = id
  {-# INLINE embed' #-}

newtype F n t = F (forall b. n b -> t b)

-- | The transformation interpreter of the embed effect. This type implements the
-- 'Embed' type class by transforming the integrated monad @n@ into another
-- integrated monad @t@ via natural transformation.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype Transformation n t m a =
  Transformation { runTransformation :: ReaderT (F n t) m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl)
    deriving (MonadBase b, MonadBaseControl b)

instance Embed' tag t m => Embed' tag n (Transformation n t m) where
  embed' na = Transformation . ReaderT $
    \(F f) -> embed' @tag (f na)
  {-# INLINE embed' #-}

-- | Runs the embed effect by transforming the integrated monad @n@ into another
-- integrated monad @t@.
--
-- @since 0.3.0.0
runEmbed' :: forall tag n t m a
           . (forall b. n b -> t b)                      -- ^ The natural transformation from monad @n@ to monad @t@.
          -> (Embed' tag n `Via` Transformation n t) m a -- ^ The program whose embed effect should be handled.
          -> m a                                         -- ^ The program with its embed effect handled.
runEmbed' f = flip runReaderT (F f) . runTransformation . runVia
{-# INLINE runEmbed' #-}

-- | The untagged version of 'runEmbed''.
makeUntagged ['runEmbed']

-- | The finalization interpreter of the embed effect. This type implements the
-- 'Embed' type class by declaring the integrated monad the final monad @m@
-- (also called the \"base monad\").
--
-- Chances are very high that you only need this interpreter if you have a
-- custom final monad because the 'Embed'' effect is already implemented for
-- final monads like 'IO', 'Maybe', @[]@ and 'Identity'.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
--
-- @since 0.3.0.0
newtype Finalization m a =
  Finalization { _runFinalization :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance Monad n => Embed' tag n (Finalization n) where
  embed' = Finalization
  {-# INLINE embed' #-}

-- | Runs the embed effect by declaring the integrated monad the final monad.
--
-- @since 0.3.0.0
runFinal' :: (Embed' tag m `Via` Finalization) m a -- ^ The program whose embed effect should be handled.
          -> m a                                   -- ^ The program with its embed effect handled.
runFinal' = coerce
{-# INLINE runFinal' #-}

-- | The untagged version of 'runFinal''.
--
-- @since 0.3.0.0
makeUntagged ['runFinal']