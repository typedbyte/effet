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
  ( -- * Embed Effect
    Embed(..)
    -- * Interpretations
  , Transformation
  , runEmbed
  ) where

-- transformers
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)

import Control.Effect.Machinery hiding (embed)

-- | An effect that integrates a monad @n@ into the computation @m@.
class Monad m => Embed n m where
  -- | Monadic actions in @n@ can be lifted into @m@ via 'embed'.
  --
  -- 'embed' is like 'liftIO', but not limited to 'IO'. In fact, 'liftIO' can
  -- be realized using 'embed' by specializing @n@ to @IO@.
  embed :: n a -> m a

makeEffect ''Embed

instance Monad m => Embed m m where
  embed = id
  {-# INLINE embed #-}

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

instance Embed t m => Embed n (Transformation n t m) where
  embed na = Transformation . ReaderT $
    \(F f) -> embed (f na)
  {-# INLINE embed #-}

-- | Runs the embed effect by transforming the integrated monad @n@ into another
-- integrated monad @t@.
runEmbed :: (forall b. n b -> t b)                 -- ^ The natural transformation from monad @n@ to monad @t@.
         -> (Embed n `Via` Transformation n t) m a -- ^ The program whose embed effect should be handled.
         -> m a                                    -- ^ The program with its embed effect handled.
runEmbed f = flip runReaderT (F f) . runTransformation . runVia
{-# INLINE runEmbed #-}