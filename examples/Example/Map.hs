-- | Examples of the map effect.
module Example.Map where

-- base
import Prelude hiding (lookup)

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.Map
import qualified Control.Effect.Map.Lazy   as L
import qualified Control.Effect.Map.Strict as S

--- Example Programs -----------------------------------------------------------

-- | Performs some inserts on the map and checks if their removal works.
removal :: Map Int String m => m Bool
removal = do
  insert 1 "Hello"
  beforeClear <- exists 1
  clear
  afterClear <- exists 1
  insert 2 "World"
  beforeDelete <- exists 2
  delete 2
  afterDelete <- exists 2
  pure $ beforeClear && not afterClear && beforeDelete && not afterDelete

-- | Performs some modifications on the map and checks the changes.
modification :: Map Int String m => m (Maybe String, Maybe String, Maybe String)
modification = do
  insert 1 "Hello"
  modify ""    (++ " World") 1
  modify "N/A" (++ " here?") 2
  h <- lookup 1
  n <- lookup 2
  x <- lookup 3
  pure (h, n, x)

multiMaps :: (Map' "a" Int String m, Map' "b" String Int m) => m (Maybe String)
multiMaps = do
  insert' @"a" 1 "Hello"
  insert' @"b" "World" 1
  maybeOne <- lookup' @"b" "World"
  case maybeOne of
    Nothing  -> pure Nothing
    Just one -> lookup' @"a" one

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates removal" $
    ( runIdentity    -- result:  Bool
    . L.runMap       -- result:  Monad m => m Bool
    $ removal )      -- effects: Map Int String
      `shouldBe` True
  it "evaluates modification" $
    ( runIdentity    -- result:  (Maybe String, Maybe String, Maybe String)
    . S.runMap       -- result:  Monad m => m (Maybe String, Maybe String, Maybe String)
    $ modification ) -- effects: Map Int String
      `shouldBe`
        (Just "Hello World", Just "N/A here?", Nothing)
  it "evaluates multiple maps" $
    ( runIdentity    -- result:  Maybe String
    . L.runMap' @"b" -- result:  Monad m => m (Maybe String)
    . S.runMap' @"a" -- effects: Map' "b" String Int
    $ multiMaps )    -- effects: Map' "a" Int String, Map' "b" String Int
      `shouldBe` Just "Hello"