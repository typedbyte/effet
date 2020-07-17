-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Machinery.Kind
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines some constraint synonyms and kinds that are used
-- throughout this library, hopefully to increase the readability of the code
-- at some points.
-----------------------------------------------------------------------------
module Control.Effect.Machinery.Kind where

-- base
import Data.Kind (Constraint, Type)

-- monad-control
import Control.Monad.Trans.Control (MonadTransControl)

-- transformers
import Control.Monad.Trans.Class (MonadTrans)

-- | The kind of monads.
type SomeMonad = Type -> Type

-- | The kind of effects, which are type classes with a monad type parameter at
-- the end.
type Effect = SomeMonad -> Constraint

-- | The kind of monad transformers, also known as effect handlers or effect
-- interpreters.
type Transformer = SomeMonad -> Type -> Type

-- | This type synonym indicates that an effect is handled by a specific monad
-- transformer.
type Handle (eff :: Effect) (t :: Transformer) m =
  eff (t m)

-- | This constraint synonym indicates that a first-order effect is not handled
-- by a specific monad transformer and must thus be delegated (\"lifted\")
-- further down the monad transformer stack in order to find its associated
-- handler.
--
-- Roughly speaking, a first-order effect is a type class whose monad type
-- parameter @m@ appears only in positive position when looking at the types of
-- its corresponding class methods (e.g., @m@ appears only in the result type).
--
-- An example of a first-order effect is the 'Control.Effect.State.State'' effect.
type Lift (eff :: Effect) (t :: Transformer) m =
  (eff m, Monad (t m), MonadTrans t)

-- | This constraint synonym indicates that a higher-order effect is not handled
-- by a specific monad transformer and must thus be delegated (\"lifted\")
-- further down the monad transformer stack in order to find its associated
-- handler.
--
-- Roughly speaking, a higher-order effect is a type class whose monad type
-- parameter @m@ appears in negative position when looking at the types of its
-- corresponding class methods (e.g., @m@ appears in the type of a method
-- parameter).
--
-- An example of a higher-order effect is the 'Control.Effect.Reader.Reader'' effect,
-- since its class method 'Control.Effect.Reader.local'' has a parameter of
-- type @m a@.
type Control (eff :: Effect) (t :: Transformer) m =
  (eff m, Monad (t m), MonadTransControl t)