{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Writer.Lazy
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Lazy interpretations of the 'Writer'' effect.
--
-- If you don't require disambiguation of multiple writer effects
-- (i.e., you only have one writer effect in your monadic context),
-- you usually need the untagged interpretations.
-----------------------------------------------------------------------------
module Control.Effect.Writer.Lazy
  ( -- * Tagged Interpretations
    execWriter'
  , runWriter'
    -- * Untagged Interpretations
  , execWriter
  , runWriter
  ) where

-- base
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT, runWriterT)

import Control.Effect.Writer    (Writer, Writer')
import Control.Effect.Machinery (Via, makeUntagged, runVia)

-- | Runs the writer effect and returns the final output.
execWriter' :: forall tag w m a. Monad m
            => (Writer' tag w `Via` WriterT w) m a -- ^ The program whose writer effect should be handled.
            -> m w                                 -- ^ The program with its writer effect handled, producing the final output @w@.
execWriter' = execWriterT . runVia
{-# INLINE execWriter' #-}

-- | Runs the writer effect and returns both the final output and the result of the interpreted program.
runWriter' :: forall tag w m a. Functor m
           => (Writer' tag w `Via` WriterT w) m a -- ^ The program whose writer effect should be handled.
           -> m (w, a)                            -- ^ The program with its writer effect handled, producing the final output @w@ and the result @a@.
runWriter' = fmap swap . runWriterT . runVia
{-# INLINE runWriter' #-}

makeUntagged ['execWriter', 'runWriter']