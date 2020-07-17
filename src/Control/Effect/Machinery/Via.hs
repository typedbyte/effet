-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Machinery.Via
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines the type 'Via' which indicates that a specific effect
-- is handled by a specific monad transformer (also known as effect handler
-- or effect interpreter).
--
-- It also defines the 'G' type, which is the global tag that is used for
-- untagged effects.
-----------------------------------------------------------------------------
module Control.Effect.Machinery.Via
  ( Via(..)
  , G
  ) where

-- base
import Control.Monad.IO.Class (MonadIO)

-- monad-control
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl,
                                    MonadTransControl, StM, defaultLiftBaseWith,
                                    defaultRestoreM,liftBaseWith, restoreM)

-- transformers
import Control.Monad.Trans.Class (MonadTrans)

-- transformers-base
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)

import Control.Effect.Machinery.Kind (Effect, Transformer)

-- | This type indicates that an effect (i.e., a type class) @eff@ is handled by
-- a specific monad transformer @t@. The type is a simple wrapper around the
-- monad transformer itself. The whole purpose of this type is to guide the type
-- system to pick the instance of type class @eff@ given by the type @t@, and
-- to delegate all other effects that are not @eff@ to their handlers which are
-- located somewhere further down the monad transformer stack.
newtype Via (eff :: Effect) (t :: Transformer) m a =
  Via { runVia :: t m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl)

instance (Monad (t m), MonadBase b m, MonadTrans t) => MonadBase b (Via eff t m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance (Monad (t m), MonadBaseControl b m, MonadTransControl t) => MonadBaseControl b (Via eff t m) where
  type StM (Via eff t m) a = ComposeSt t m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

-- | This type is used as tag for all untagged effects. In order words, every
-- effect is tagged, even untagged ones, but all the untagged ones simply have
-- the same tag @G@ (short for \"Global\", because you can view tags as some
-- kind of namespace mechanism, and all untagged effects live in the same
-- global namespace).
--
-- If you don\'t want to use tagged effects (i.e., you write effect type classes
-- without a tag type parameter), you can ignore this type completely.
data G