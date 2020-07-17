-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.Machinery
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports all the definitions needed to define effects and their
-- corresponding handlers. Usually, importing this module is everything you
-- need to get started.
-----------------------------------------------------------------------------
module Control.Effect.Machinery
  ( -- * Re-exports from @effet@
    module Control.Effect.Machinery.Default
  , module Control.Effect.Machinery.Kind
  , module Control.Effect.Machinery.Tagger
  , module Control.Effect.Machinery.TH
  , module Control.Effect.Machinery.Via
    -- * Re-exports from @base@
  , module Control.Monad.IO.Class
    -- * Re-exports from @monad-control@
  , module Control.Monad.Trans.Control
    -- * Re-exports from @transformers@
  , module Control.Monad.Trans.Class
    -- * Re-exports from @transformers-base@
  , module Control.Monad.Base
  ) where

-- base
import Control.Monad.IO.Class

-- monad-control
import Control.Monad.Trans.Control -- hiding (embed)

-- transformers
import Control.Monad.Trans.Class

-- transformers-base
import Control.Monad.Base

import Control.Effect.Machinery.Default
import Control.Effect.Machinery.Kind
import Control.Effect.Machinery.Tagger
import Control.Effect.Machinery.TH
import Control.Effect.Machinery.Via