-- | Examples of the state effect.
module Example.State where

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.State
import qualified Control.Effect.State.Lazy   as L
import qualified Control.Effect.State.Strict as S

--- Example Programs -----------------------------------------------------------

-- | Increments the state.
increment :: (Num n, State n m) => m ()
increment = modify (+1)

-- | Increments the state tagged "s" by 3 and adds up the old and new values.
incrementWithResult :: State' "s" Int m => m String
incrementWithResult = do
  before <- get' @"s"
  modify' @"s" (+3)
  after <- get' @"s"
  pure . show $ before + after

-- | Increments two elements in a tuple.
incrementBoth :: (Num n1, Num n2, State (n1,n2) m) => m ()
incrementBoth = modify (\(x,y) -> (x+1, y+1))

-- | Increments the state tagged "foo" by 1,
--   increments the state tagged "bar" by 2,
--   then adds up the two states.
twoStates :: (State' "foo" Int m, State' "bar" Int m) => m Int
twoStates = do
  tagState' @"foo" $ increment
  tagState' @"bar" $ increment >> increment
  foo <- get' @"foo"
  bar <- get' @"bar"
  pure $ foo + bar

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates increment" $
    ( runIdentity          -- result:  Int
    . L.execState 5        -- result:  (Num n, Monad m) => m n
    $ increment            -- effects: Num n => State n
    ) `shouldBe` (6::Int)
  it "evaluates incrementWithResult" $
    ( runIdentity          -- result:  (Int, String)
    . S.runState' @"s" 5   -- result:  Monad m => m (Int, String)
    $ incrementWithResult  -- effects: State' "s" Int
    ) `shouldBe` (8, "13")
  it "evaluates incrementBoth" $
    ( runIdentity          -- result:  (Int, Int)
    . L.execState (1,2)    -- result:  (Num n1, Num n2, Monad m) => m (n1, n2)
    $ incrementBoth        -- effects: (Num n1, Num n2) => State (n1, n2)
    ) `shouldBe` (2::Int, 3::Int)
  it "evaluates twoStates" $
    ( runIdentity          -- result:  (Int, (Int, Int))
    . L.runState' @"foo" 3 -- result:  Monad m => m (Int, (Int, Int))
    . S.runState' @"bar" 5 -- effects: State' "foo" Int
    $ twoStates            -- effects: State' "foo" Int, State' "bar" Int
    ) `shouldBe` (4, (7, 11))