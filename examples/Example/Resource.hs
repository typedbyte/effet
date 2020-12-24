-- | Examples of the resource effect.
module Example.Resource where

-- base
import qualified Control.Exception as E
import Prelude hiding (print)

-- hspec
import Test.Hspec (Spec, it)

-- effet
import Control.Effect.Error
import Control.Effect.Resource

import Hspec (print, shouldPrint)

--- Example Programs -----------------------------------------------------------

-- | Type used here as virtual handle.
newtype Handle = Handle { nameOf :: String }

-- | Simple bracket with print outputs.
aBracket :: Resource m => String -> m ()
aBracket name = do
  bracket
    ( print ("Alloc " ++ name) >> pure (Handle name) )
    ( \handle -> print $ "Free " ++ nameOf handle )
    ( \handle -> print $ "Use "  ++ nameOf handle )

-- | Simple bracket with print outputs.
aBracketOnError :: Resource m => String -> m ()
aBracketOnError name = do
  bracketOnError
    ( print ("Alloc " ++ name) >> pure (Handle name) )
    ( \handle -> print $ "Free " ++ nameOf handle )
    ( \handle -> print $ "Use "  ++ nameOf handle )

-- | Bracket where the usage function throws an ArrayException.
errorBracket :: (Error String m, Resource m) => String -> m ()
errorBracket name = do
  bracket
    ( print ("Alloc " ++ name) >> pure (Handle name) )
    ( \handle -> print $ "Free " ++ nameOf handle )
    ( \handle -> E.throw (E.UndefinedElement $ nameOf handle ) )

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates a bracket" $
    ( runResourceIO         -- result:  (MonadBaseControl IO m, MonadIO m) => m (),
                            --          unified with IO ()
    $ aBracket "X" )        -- effects: Resource
      `shouldPrint`
        "\"Alloc X\"\n\"Use X\"\n\"Free X\"\n"
  it "evaluates a bracket without freeing" $
    ( runResourceIO         -- result:  (MonadBaseControl IO m, MonadIO m) => m (),
                            --          unified with IO ()
    $ aBracketOnError "X" ) -- effects: Resource
      `shouldPrint`
        "\"Alloc X\"\n\"Use X\"\n"
  it "evaluates a bracket with an error" $
    ( runResourceIO         -- result:  (MonadBaseControl IO m, MonadIO m) => m (Either String ()),
                            --          unified with IO (Either String ())
    . runError              -- effects: Resource
    $ errorBracket "X" )    -- effects: Error String, Resource
      `E.catch`
        ( \(_ :: E.ArrayException) -> pure (Left "Intended error") )
      `shouldPrint`
        "\"Alloc X\"\n\"Free X\"\n"