-- | Examples of the writer effect.
module Example.Writer where

-- base
import Data.Monoid (Sum(Sum), getSum)

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.State
import Control.Effect.Writer
import qualified Control.Effect.State.Lazy    as L
import qualified Control.Effect.Writer.Lazy   as L
import qualified Control.Effect.Writer.Strict as S

--- Example Programs -----------------------------------------------------------

-- | Write something in a sub-action, capture it and tell something again.
sumWriter :: Writer (Sum Int) m => m Int
sumWriter = do
  (w,a) <- listen $ do tell 5
                       pure 8
  tell w
  tell a
  pure $ getSum (w + a)

-- | Combines the state and writer effect.
stateWriter :: (State Int m, Writer [Int] m) => m Int
stateWriter = do
  tell [1,2,3]
  num <- get
  tell [num, num + 1, num + 2]
  pure num

-- | Censors (modifies) the output of sumWriter.
censorWriter :: Writer (Sum Int) m => m Int
censorWriter = censor (fmap (+1)) sumWriter

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "listens to sub-action writings" $
    ( runIdentity    -- result:  (Sum Int, Int)
    . S.runWriter    -- result:  Monad m => m (Sum Int, Int)
    $ sumWriter )    -- effects: Writer (Sum Int)
      `shouldBe`
        (Sum 18, 13)
  it "combines state and writer" $
    ( runIdentity    -- result:  ([Int], Int)
    . L.runWriter    -- result:  Monad m => m ([Int], Int)
    . L.execState 4  -- effects: Writer [Int]
    $ stateWriter )  -- effects: State Int, Writer [Int]
      `shouldBe`
        ([1,2,3,4,5,6], 4)
  it "censors/modifies output" $
    ( runIdentity    -- result:  (Sum Int, Int)
    . L.runWriter    -- result:  Monad m => m (Sum Int, Int)
    $ censorWriter ) -- effects: Writer (Sum Int)
      `shouldBe`
        (Sum 19, 13)