-- | Examples of the embed effect.
module Example.Embed where

-- base
import Control.Monad  (guard)
import Data.Maybe     (listToMaybe, maybeToList)
import Prelude hiding (print)

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Embed
import Control.Effect.Reader

import Hspec (print, shouldPrint)

--- Example Programs -----------------------------------------------------------

-- | Find shared numbers of two reader-based lists using the list monad.
intersect :: (Reader' "xs" [Int] m, Reader' "ys" [Int] m, Embed [] m) => m Int
intersect = do
  xs <- ask' @"xs"
  ys <- ask' @"ys"
  embed $ do
    x <- xs
    y <- ys
    guard $ x == y
    pure x

-- | Divide two numbers using the maybe monad.
divide :: Embed Maybe m => Maybe Int -> Maybe Int -> m Int
divide mx my = embed $ do
  x <- mx
  y <- my
  guard $ y /= 0
  pure  $ x `div` y

-- | Performs multiple divisions.
divisions :: Embed [] m => Maybe Int -> m Int
divisions divisor = do
  a <- runEmbed maybeToList $ divide (Just 3) (Just 1)
  b <- runEmbed maybeToList $ divide (Just 2) divisor
  c <- runEmbed maybeToList $ divide (Just 5) divisor
  d <- runEmbed maybeToList $ divide (Just 9) (Just 4)
  embed [a,b,c,d]

-- | Uses embed instead of liftIO to print texts with prefix and suffix (reader).
simulateLiftIO :: (Embed IO m, Reader String m) => String -> m ()
simulateLiftIO prefix = do
  suffix <- ask
  embed $ do
    print $ prefix ++ "Hello" ++ suffix
    print $ prefix ++ "World" ++ suffix

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates intersect with []" $
    ( runReader' @"ys" [2,3,4]    -- effects: Embed [], result: [Int]
    . runReader' @"xs" [1,2,3]    -- effects: Reader' "ys" [Int], Embed []
    $ intersect )                 -- effects: Reader' "xs" [Int], Reader' "ys" [Int], Embed []
      `shouldBe` [2,3]
  it "evaluates intersect with Maybe" $
    ( runEmbed listToMaybe        -- effects: Embed Maybe, result: Maybe Int
    . runReader' @"ys" [2,3,4]    -- effects: Embed []
    . runReader' @"xs" [1,2,3]    -- effects: Reader' "ys" [Int], Embed []
    $ intersect )                 -- effects: Reader' "xs" [Int], Reader' "ys" [Int], Embed []
      `shouldBe` Just 2
  it "evaluates division" $
    ( divide (Just 10) (Just 3) ) -- effects: Embed Maybe, result: Maybe Int
      `shouldBe` Just 3
  it "evaluates division by zero" $
    ( divide (Just 2) (Just 0) )  -- effects: Embed Maybe, result: Maybe Int
      `shouldBe` Nothing
  it "evaluates divisions" $
    ( divisions (Just 2) )        -- effects: Embed [], result: [Int]
      `shouldBe` [3,1,2,2]
  it "evaluates divisions by zero" $
    ( divisions Nothing )         -- effects: Embed Maybe, result: Maybe [Int]
      `shouldBe` []
  it "evaluates simulateLiftIO" $
    ( runReader " [S]"            -- effects: Embed IO, result: IO ()
    $ simulateLiftIO "[P] " )     -- effects: Embed IO, Reader String
      `shouldPrint`
        "\"[P] Hello [S]\"\n\"[P] World [S]\"\n"