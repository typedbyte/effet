-- | Examples of the error effect.
module Example.Error where

-- base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude         hiding (print)

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Error
import Control.Effect.Identity
import Control.Effect.Reader

import Hspec (print, shouldPrint)

--- Example Programs -----------------------------------------------------------

data DivideByZero = DivideByZero
  deriving (Eq, Show)

-- | Divides two numbers, may yield an error when dividing by zero.
divide :: Error DivideByZero m => Int -> Int -> m Int
divide _ 0 = throwError DivideByZero
divide x y = pure $ x `div` y

-- | Divides two numbers and returns a default value in case of zero division.
catchDivide :: Error DivideByZero m => Int -> Int -> m Int
catchDivide x y =
  catchError
    ( divide x y )
    ( \_ -> pure 1337 )

-- | Divides two numbers found in the readers, catches the division by zero
-- error and prints a String describing the result.
divideByReader :: ( Error DivideByZero m
                  , MonadIO m
                  , Reader' "numerator" Int m
                  , Reader' "divisor" Int m )
               => m ()
divideByReader = do
  n <- ask' @"numerator"
  d <- ask' @"divisor"
  catchError
    ( do r <- divide n d
         liftIO . print $ "Result: " ++ show r )
    ( \e ->
         liftIO . print $ "Error: "  ++ show e )

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "divides normally" $
    ( runIdentity                -- result:  Either DivideByZero Int
    . runError                   -- result:  Monad m => m (Either DivideByZero Int)
    $ divide 8 3 )               -- effects: Error DivideByZero
      `shouldBe` Right 2
  it "divides by zero" $
    ( runIdentity                -- result:  Either DivideByZero Int
    . runError                   -- result:  Monad m => m (Either DivideByZero Int)
    $ divide 5 0 )               -- effects: Error DivideByZero
      `shouldBe` Left DivideByZero
  it "catches division by zero" $
    ( runIdentity                -- result:  Either DivideByZero Int
    . runError                   -- result:  Monad m => m (Either DivideByZero Int)
    $ catchDivide 5 0 )          -- effects: Error DivideByZero
      `shouldBe` Right 1337
  it "catches normally" $
    ( runReader' @"divisor" 3    -- result:  MonadIO m => m (Either DivideByZero ()),
                                 --          unified with IO (Either DivideByZero ())
    . runReader' @"numerator" 17 -- effects: Reader' "divisor", MonadIO
    . runError                   -- effects: Reader' "numerator", Reader' "divisor", MonadIO
    $ divideByReader )           -- effects: Error DivideByZero, Reader' "numerator",
                                 --          Reader' "divisor", MonadIO
      `shouldPrint` "\"Result: 5\"\n"
  it "catches division by zero" $
    ( runReader' @"divisor" 0    -- result:  MonadIO m => m (Either DivideByZero ()),
                                 --          unified with IO (Either DivideByZero ())
    . runReader' @"numerator" 17 -- effects: Reader' "divisor", MonadIO
    . runError                   -- effects: Reader' "numerator", Reader' "divisor", MonadIO
    $ divideByReader )           -- effects: Error DivideByZero, Reader' "numerator",
                                 --          Reader' "divisor", MonadIO
      `shouldPrint` "\"Error: DivideByZero\"\n"