{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Writer
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The writer effect, similar to the @MonadWriter@ type class from the @mtl@
-- library.
--
-- Lazy and strict interpretations of the effect are available here:
-- "Control.Effect.Writer.Lazy" and "Control.Effect.Writer.Strict".
-----------------------------------------------------------------------------
module Control.Effect.Writer
  ( -- * Tagged Writer Effect
    Writer'(..)
    -- * Convenience Functions
  , listens'
    -- * Untagged Writer Effect
    -- | If you don't require disambiguation of multiple writer effects
    -- (i.e., you only have one writer effect in your monadic context),
    -- it is recommended to always use the untagged writer effect.
  , Writer
  , tell
  , listen
  , censor
  , listens
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged writer effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagWriter'' \@\"newTag\" program
    --     'retagWriter'' \@\"oldTag\" \@\"newTag\" program
    --     'untagWriter'' \@\"erasedTag\" program
    -- @
    -- 
  , tagWriter'
  , retagWriter'
  , untagWriter'
  ) where

-- base
import Data.Tuple (swap)

-- transformers
import qualified Control.Monad.Trans.RWS.CPS     as Strict
import qualified Control.Monad.Trans.RWS.Lazy    as Lazy
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.CPS  as S

import Control.Effect.Machinery

-- | An effect that adds a write-only, accumulated output to a given computation.
class (Monad m, Monoid w) => Writer' tag w m | tag m -> w where
  -- | Produces the output @w@. In other words, @w@ is appended to the accumulated output.
  tell' :: w -> m ()
  -- | Executes a sub-computation and appends @w@ to the accumulated output.
  listen' :: m a -> m (w, a)
  -- | Executes a sub-computation and applies the function to its output.
  censor' :: (w -> w) -- ^ The function which is applied to the output.
          -> m a      -- ^ The sub-computation which produces the modified output.
          -> m a      -- ^ The result of the sub-computation.

makeTaggedEffect ''Writer'

instance (Monad m, Monoid w) => Writer' tag w (L.WriterT w m) where
  tell' = L.tell
  {-# INLINE tell' #-}
  listen' = fmap swap . L.listen
  {-# INLINE listen' #-}
  censor' = L.censor
  {-# INLINE censor' #-}

instance (Monad m, Monoid w) => Writer' tag w (S.WriterT w m) where
  tell' = S.tell
  {-# INLINE tell' #-}
  listen' = fmap swap . S.listen
  {-# INLINE listen' #-}
  censor' = S.censor
  {-# INLINE censor' #-}

instance (Monad m, Monoid w) => Writer' tag w (Lazy.RWST r w s m) where
  tell' = Lazy.tell
  {-# INLINE tell' #-}
  listen' = fmap swap . Lazy.listen
  {-# INLINE listen' #-}
  censor' = Lazy.censor
  {-# INLINE censor' #-}

instance (Monad m, Monoid w) => Writer' tag w (Strict.RWST r w s m) where
  tell' = Strict.tell
  {-# INLINE tell' #-}
  listen' = fmap swap . Strict.listen
  {-# INLINE listen' #-}
  censor' = Strict.censor
  {-# INLINE censor' #-}

-- | Executes a sub-computation and applies the function to its output, thus adding
-- an additional value to the result of the sub-computation.
listens' :: forall tag w b m a. Writer' tag w m
         => (w -> b) -- ^ The function which is applied to the output.
         -> m a      -- ^ The sub-computation which produces the modified output.
         -> m (b, a) -- ^ The result of the sub-computation, including the modified output.
listens' f action = do
  ~(w, a) <- listen' @tag action
  pure (f w, a)
{-# INLINE listens' #-}

makeUntagged ['listens']