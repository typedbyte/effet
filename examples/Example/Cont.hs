-- | Examples of the continuation effect.
module Example.Cont where

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Cont
import Control.Effect.Identity

import Example.Logger

--- Example Programs -----------------------------------------------------------

fiveTimesCont :: (Cont m, Logger m) => m ()
fiveTimesCont = do
    logStr "x"
    (k, num) <- callCC $ \k -> let f x = k (f, x)
                               in pure (f, 0::Int)
    logStr "y"
    logStr "z"
    if num < 5
      then k (num + 1) >> pure ()
      else logStr $ show num

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates fiveTimesCont" $
    ( runIdentity      -- result:  ((), [String])
    . runCollectLogger -- result:  Monad m => m ((), [String])
    . evalCont         -- effects: Logger
    $ fiveTimesCont    -- effects: Cont, Logger
    ) `shouldBe`
        ((), ["5","z","y","z","y","z","y","z","y","z","y","z","y","x"])