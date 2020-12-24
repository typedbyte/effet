-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Machinery.Via
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines the types 'EachVia' and its corresponding type synonym
-- 'Via' which indicate that specific effects are handled by a specific monad
-- transformer (also known as effect handler or effect interpreter).
--
-- It also defines the 'G' type, which is the global tag that is used for
-- untagged effects.
-- 
-- Last but not least, it defines some constraint synonyms and kinds that are
-- used throughout this library, hopefully to increase the readability of the
-- code at some points.
-----------------------------------------------------------------------------
module Control.Effect.Machinery.Via
  ( -- * Core Types
    EachVia(..)
  , Via
  , G
    -- * Constraint Synonyms and Kinds
  , SomeMonad
  , Effect
  , Transformer
  , Handle
  , Find
  , Lift
  , Control
    -- * Convenience Functions
  , Expand
  ) where

-- base
import Control.Monad.IO.Class (MonadIO)
import Data.Kind              (Constraint, Type)

-- monad-control
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl,
                                    MonadTransControl, StM, defaultLiftBaseWith,
                                    defaultRestoreM,liftBaseWith, restoreM)

-- transformers
import Control.Monad.Trans.Class (MonadTrans)

-- transformers-base
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)

-- | This type indicates that the effects (i.e., type classes) @effs@ are handled by
-- a specific monad transformer @t@. The type is a simple wrapper around the
-- monad transformer itself. The whole purpose of this type is to guide the type
-- system to pick the instances of type classes @effs@ given by the type @t@, and
-- to delegate all other effects that are not in @effs@ to their handlers which are
-- located somewhere further down the monad transformer stack.
--
-- @since 0.2.0.0
newtype EachVia (effs :: [Effect]) (t :: Transformer) m a =
  EachVia { runVia :: t m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl)

instance (Monad (t m), MonadBase b m, MonadTrans t) => MonadBase b (EachVia effs t m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance (Monad (t m), MonadBaseControl b m, MonadTransControl t) => MonadBaseControl b (EachVia effs t m) where
  type StM (EachVia effs t m) a = ComposeSt t m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

-- | This type synonym can be used to indicate that a single effect @eff@ is
-- handled by a specific monad transformer @t@.
--
-- @since 0.2.0.0
type Via eff t m a = EachVia '[eff] t m a

-- | This type is used as tag for all untagged effects. In order words, every
-- effect is tagged, even untagged ones, but all the untagged ones simply have
-- the same tag @G@ (short for \"Global\", because you can view tags as some
-- kind of namespace mechanism, and all untagged effects live in the same
-- global namespace).
--
-- If you don\'t want to use tagged effects (i.e., you write effect type classes
-- without a tag type parameter), you can ignore this type completely.
data G

-- | The kind of monads.
type SomeMonad = Type -> Type

-- | The kind of effects, which are type classes with a monad type parameter at
-- the end.
type Effect = SomeMonad -> Constraint

-- | The kind of monad transformers, also known as effect handlers or effect
-- interpreters.
type Transformer = SomeMonad -> Type -> Type

-- | This constraint synonym indicates that an effect is handled by a specific monad
-- transformer.
--
-- @since 0.4.0.0
type Handle (cxt :: [Effect]) (eff :: Effect) (others :: [Effect]) (t :: Transformer) m =
  (eff (t m), Expand cxt (eff ': others) t m)

-- | This constraint synonym indicates that an effect @eff@ is not at the head of the
-- type level list of effects to be handled, so the effect must be found further
-- down in the tail @effs@.
--
-- @since 0.4.0.0
type Find (cxt :: [Effect]) (eff :: Effect) (other :: Effect) (effs :: [Effect]) (t :: Transformer) m =
  (eff (EachVia effs t m), Expand cxt (other ': effs) t m)

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
--
-- @since 0.4.0.0
type Lift (cxt :: [Effect]) (eff :: Effect) (t :: Transformer) m =
  (eff m, Expand cxt '[] t m, MonadTrans t)

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
--
-- @since 0.4.0.0
type Control (cxt :: [Effect]) (eff :: Effect) (t :: Transformer) m =
  (eff m, Expand cxt '[] t m, Monad (t m), MonadTransControl t)

-- | Type-level helper function to apply a list of constraints (i.e., effects)
-- to 'EachVia'. One should not call this by hand, it will be used by the generated
-- code or called by 'Handle', 'Find', 'Lift' and 'Control'.
--
-- @since 0.4.0.0
type family Expand (cxt :: [Effect]) (effs :: [Effect]) (t :: Transformer) m :: Constraint where
  Expand '[] effs t m = ()
  Expand (cxt ': cxts) effs t m = (cxt (EachVia effs t m), Expand cxts effs t m)