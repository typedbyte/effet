-- | Examples of the RWS effect.
module Example.RWS where

-- base
import Data.Monoid (Sum(Sum))

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.Reader
import Control.Effect.RWS
import Control.Effect.State
import qualified Control.Effect.State.Lazy    as L
import qualified Control.Effect.Writer.Strict as S
import qualified Control.Effect.RWS.Lazy      as L

import Example.Reader (triple)
import Example.State  (increment)
import Example.Writer (sumWriter)

--- Example Programs -----------------------------------------------------------

-- | Combine reader, writer and state examples from the other test cases.
combineRWS :: RWS Int (Sum Int) Int m => m (Int, Int, Int)
combineRWS = do
  r <- triple
  w <- sumWriter
  increment
  s <- get
  pure (r, w, s)

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "combines reader/write/state test cases" $
    ( runIdentity     -- result:  (Sum Int, Int, (Int, Int, Int))
    . L.runRWS 20 5   -- result:  Monad m => m (Sum Int, Int, (Int, Int, Int))
    $ combineRWS )    -- effects: RWS Int (Sum Int) Int
      `shouldBe`
        (Sum 18, 6, (60, 13, 6))
  it "separates reader/write/state components" $
    ( runIdentity     -- result:  (Int, (Sum Int, (Int, Int, Int)))
    . L.runState 3    -- result:  Monad m => m (Int, (Sum Int, (Int, Int, Int)))
    . S.runWriter     -- effects: State Int
    . runReader 15    -- effects: Writer (Sum Int), State Int
    . runSeparatedRWS -- effects: Reader Int, Writer (Sum Int), State Int
    $ combineRWS )    -- effects: RWS Int (Sum Int) Int
      `shouldBe`
        (4, (Sum 18, (45, 13, 4)))