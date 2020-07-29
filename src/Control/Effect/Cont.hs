{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Cont
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The continuation effect, similar to the @MonadCont@ type class from the
-- @mtl@ library.
-----------------------------------------------------------------------------
module Control.Effect.Cont
  ( -- * Tagged Continuation Effect
    Cont'(..)
    -- * Untagged Continuation Effect
    -- | If you don't require disambiguation of multiple continuation effects
    -- (i.e., you only have one continuation effect in your monadic context),
    -- it is recommended to always use the untagged continuation effect.
  , Cont
  , callCC
    -- * Interpretations
  , runCont'
  , runCont
  , evalCont'
  , evalCont
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged continuation effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagCont'' \@\"newTag\" program
    --     'retagCont'' \@\"oldTag\" \@\"newTag\" program
    --     'untagCont'' \@\"erasedTag\" program
    -- @
    -- 
  , tagCont'
  , retagCont'
  , untagCont'
  ) where

-- transformers
import qualified Control.Monad.Trans.Cont as C

import Control.Effect.Machinery

-- | An effect that adds an abortive continuation to a computation.
class Monad m => Cont' tag m where
  -- | Adapted from the @mtl@ library documentation:
  --
  -- @callCC'@ (call-with-current-continuation) calls a function with the
  -- current continuation as its argument. Provides an escape continuation
  -- mechanism for use with continuation monads. Escape continuations allow to
  -- abort the current computation and return a value immediately. They achieve
  -- a similar result to 'Control.Effect.Error.throwError'' and
  -- 'Control.Effect.Error.catchError'' of the 'Control.Effect.Error.Error''
  -- effect. Advantage of this function over calling @return@ is that it makes
  -- the continuation explicit, allowing more flexibility and better control.
  --
  -- The standard idiom used with @callCC'@ is to provide a lambda-expression to
  -- name the continuation. Then calling the named continuation anywhere within
  -- its scope will escape from the computation, even if it is many layers deep
  -- within nested computations.
  callCC' :: ((a -> m b) -> m a) -> m a

makeHandler ''Cont'
makeFinder  ''Cont'
makeTagger  ''Cont'

instance Control (Cont' tag) t m => Cont' tag (EachVia '[] t m) where
  callCC' f =
    liftWith
      ( \run -> callCC' @tag $ \c -> run . f $
          \a -> lift (run (pure a) >>= c)
      )
      >>= restoreT . pure
  {-# INLINEABLE callCC' #-}

instance Cont' tag (C.ContT r m) where
  callCC' = C.callCC
  {-# INLINE callCC' #-}

-- | Runs the continuation effect with a given final continuation.
runCont' :: forall tag r m a. (a -> m r) -> (Cont' tag `Via` C.ContT r) m a -> m r
runCont' f = flip C.runContT f . runVia
{-# INLINE runCont' #-}

-- | The untagged version of 'runCont''.
runCont :: (a -> m r) -> (Cont `Via` C.ContT r) m a -> m r
runCont = runCont' @G
{-# INLINE runCont #-}

-- | Runs the continuation effect with 'pure' as final continuation.
evalCont' :: forall tag r m. Applicative m => (Cont' tag `Via` C.ContT r) m r -> m r
evalCont' = runCont' pure
{-# INLINE evalCont' #-}

-- | The untagged version of 'evalCont''.
evalCont :: Applicative m => (Cont `Via` C.ContT r) m r -> m r
evalCont = evalCont' @G
{-# INLINE evalCont #-}