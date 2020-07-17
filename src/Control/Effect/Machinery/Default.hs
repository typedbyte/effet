-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Machinery.Default
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides default implementations for the 'MonadTrans' and
-- 'MonadTransControl' type classes.
-----------------------------------------------------------------------------
module Control.Effect.Machinery.Default (Default(..)) where

-- base
import Control.Monad.IO.Class (MonadIO)

-- monad-control
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl, StT,
                                    liftWith, restoreT)

-- transformers
import Control.Monad.Trans.Class (MonadTrans, lift)

-- transformers-base
import Control.Monad.Base (MonadBase)

-- | This type provides default implementations for the 'MonadTrans' and
-- 'MonadTransControl' type classes. The type is intended to be targeted by the
-- @DerivingVia@ language extension when writing an effect handler newtype that
-- wraps a monad @m a@ :
--
-- @
--     newtype MyHandler m a =
--       MyHandler { runMyHandler :: m a }
--         deriving (Applicative, Functor, Monad, MonadIO)
--         deriving (MonadTrans, MonadTransControl) via 'Default'
--         deriving (MonadBase b, MonadBaseControl b)
-- @
newtype Default m a =
  Default { runDefault :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadBase b, MonadBaseControl b)

instance MonadTrans Default where
  lift = Default
  {-# INLINE lift #-}

instance MonadTransControl Default where
  type StT Default a = a
  liftWith f = Default (f runDefault)
  {-# INLINE liftWith #-}
  restoreT = Default
  {-# INLINE restoreT #-}