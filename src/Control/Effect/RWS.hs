{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Effect.RWS
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- The effect that combines the reader, writer and state effect, similar to the
-- @MonadRWS@ type class from the @mtl@ library.
--
-- Lazy and strict interpretations of the effect are available here:
-- "Control.Effect.RWS.Lazy" and "Control.Effect.RWS.Strict".
-----------------------------------------------------------------------------
module Control.Effect.RWS
  ( -- * Tagged RWS Effect
    RWS'
    -- * Untagged RWS Effect
    -- | If you don't require disambiguation of multiple RWS effects
    -- (i.e., you only have one RWS effect in your monadic context),
    -- it is recommended to always use the untagged RWS effect.
  , RWS
    -- * Interpretations
  , Separation
  , runSeparatedRWS'
  , runSeparatedRWS
    -- * Tagging and Untagging
    -- | Conversion functions between the tagged and untagged RWS effect,
    -- usually used in combination with type applications, like:
    --
    -- @
    --     'tagRWS'' \@\"newTag\" program
    --     'retagRWS'' \@\"oldTag\" \@\"newTag\" program
    --     'untagRWS'' \@\"erasedTag\" program
    -- @
    -- 
  , tagRWS'
  , retagRWS'
  , untagRWS'
  ) where

-- base
import Data.Coerce (coerce)

-- transformers
import qualified Control.Monad.Trans.RWS.CPS  as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy

import qualified Control.Effect.Reader as R
import qualified Control.Effect.State  as S
import qualified Control.Effect.Writer as W

import Control.Effect.Machinery hiding (Tagger)

-- | An effect that adds the following features to a given computation:
--
--     * (R) an immutable environment (the \"reader\" part)
--     * (W) a write-only, accumulated output (the \"writer\" part)
--     * (S) a mutable state (the \"state\" part)
--
-- @since 0.2.0.0
class (R.Reader' tag r m, W.Writer' tag w m, S.State' tag s m) => RWS' tag r w s m | tag m -> r w s

makeTaggedEffect ''RWS'

instance (Monad m, Monoid w) => RWS' tag r w s (Lazy.RWST r w s m)
instance (Monad m, Monoid w) => RWS' tag r w s (Strict.RWST r w s m)

-- | The separation interpreter of the RWS effect. This type implements the 'RWS''
-- type class by splitting the effect into separate 'R.Reader'', 'W.Writer'' and
-- 'S.State'' effects which can then be interpreted individually.
--
-- When interpreting the effect, you usually don\'t interact with this type directly,
-- but instead use one of its corresponding interpretation functions.
newtype Separation m a =
  Separation { runSeparation :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

-- The following three "boring" instances are needed by hand, since GHC 8.6 cannot
-- derive them. With newer GHCs, you can derive these instances by adding above:
-- deriving (R.Reader' tag r, W.Writer' tag w, S.State' tag s)
instance R.Reader' tag r m => R.Reader' tag r (Separation m) where
  ask' = Separation (R.ask' @tag)
  {-# INLINE ask' #-}
  local' f m = Separation (R.local' @tag f (runSeparation m))
  {-# INLINE local' #-}
  reader' f = Separation (R.reader' @tag f)
  {-# INLINE reader' #-}

instance W.Writer' tag w m => W.Writer' tag w (Separation m) where
  tell' w = Separation (W.tell' @tag w)
  {-# INLINE tell' #-}
  listen' m = Separation (W.listen' @tag (runSeparation m))
  {-# INLINE listen' #-}
  censor' f m = Separation (W.censor' @tag f (runSeparation m))
  {-# INLINE censor' #-}

instance S.State' tag s m => S.State' tag s (Separation m) where
  get' = Separation (S.get' @tag)
  {-# INLINE get' #-}
  put' s = Separation (S.put' @tag s)
  {-# INLINE put' #-}
  state' f = Separation (S.state' @tag f)
  {-# INLINE state' #-}

instance (R.Reader' tag r m, W.Writer' tag w m, S.State' tag s m) => RWS' tag r w s (Separation m)

-- | Runs the RWS effect via separation.
runSeparatedRWS'
  :: ('[RWS' tag r w s, R.Reader' tag r, W.Writer' tag w, S.State' tag s] `EachVia` Separation) m a
  -- ^ The program whose RWS effect should be handled.
  -> m a
  -- ^ The program with its RWS effect handled.
runSeparatedRWS' = coerce
{-# INLINE runSeparatedRWS' #-}

-- | The untagged version of 'runSeparatedRWS''.
runSeparatedRWS :: ('[RWS r w s, R.Reader r, W.Writer w, S.State s] `EachVia` Separation) m a -> m a
runSeparatedRWS = coerce
{-# INLINE runSeparatedRWS #-}