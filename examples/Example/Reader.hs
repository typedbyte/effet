-- | Examples of the reader effect.
module Example.Reader where

-- base
import Control.Monad.IO.Class (MonadIO)
import Prelude         hiding (print)

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.Reader

import Hspec (print, shouldPrint)

--- Example Programs -----------------------------------------------------------

-- | Returns the triple of the number in the reader.
triple :: Reader Int m => m Int
triple = do
  single <- ask
  double <- asks (*2)
  pure $ single + double

-- | Prints the triple and sixfold of the number in the reader.
printTripleSix :: (MonadIO m, Reader Int m) => m ()
printTripleSix = do
  t <- triple
  s <- local (*2) triple
  print t
  print s

-- | Prints the double of reader "bar" and the triple of reader "foo".
printFooBar :: (MonadIO m, Reader' "bar" Int m, Reader' "foo" Int m) => m ()
printFooBar =
  local' @"bar" (*2) $
  local' @"foo" (*3) $ do
    ask' @"bar" >>= print
    ask' @"foo" >>= print

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates triple" $
    ( runIdentity          -- result:  Int
    . runReader 22         -- result:  Monad m => m Int
    $ triple )             -- effects: Reader Int
      `shouldBe` 66
  it "evaluates printTripleSix" $
    runReader 10           -- result:  MonadIO m => m (), unified with IO ()
    printTripleSix         -- effects: MonadIO, Reader Int
      `shouldPrint` "30\n60\n"
  it "evaluates printFooBar" $
    ( runReader' @"bar" 20 -- result:  MonadIO m => m (), unified with IO ()
    . runReader' @"foo" 50 -- effects: MonadIO, Reader' "bar" Int
    $ printFooBar          -- effects: MonadIO, Reader' "bar" Int, Reader' "foo" Int
    ) `shouldPrint` "40\n150\n"