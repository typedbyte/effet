-- | Examples of the managed effect.
module Example.Managed where

-- base
import Prelude hiding (print)

-- hspec
import Test.Hspec (Spec, it)

-- effet
import Control.Effect.Error
import Control.Effect.Managed

import Hspec (print, shouldPrint)

--- Example Programs -----------------------------------------------------------

-- | Type used here as virtual handle.
newtype Handle = Handle { nameOf :: String }

-- | Gets a managed virtual handle specified by a name.
getHandle :: Managed m => String -> m Handle
getHandle name = manage
  ( print ("Alloc " ++ name) >> pure (Handle name) )
  ( \handle -> print $ "Free " ++ nameOf handle )

-- | Simple program that makes use of two handles. Handles are destroyed
-- automatically at the end of the program.
useHandles :: Managed m => m ()
useHandles = do
  a <- getHandle "A"
  b <- getHandle "B"
  print $ "Use " ++ nameOf a
  print $ "Use " ++ nameOf b

-- | Allocates some handlers, then throws an error.
throwCheck :: (Error String m, Managed m) => m ()
throwCheck = do
  a <- getHandle "A"
  b <- getHandle "B"
  print $ "Use " ++ nameOf a
  _ <- throwError "Some error"
  print $ "Use " ++ nameOf b

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "manages a handle" $
    ( runManaged      -- result:  (MonadBaseControl IO m, MonadIO m) => m (),
                      --          unified with IO ()
    $ getHandle "X" ) -- effects: Managed
      `shouldPrint`
        "\"Alloc X\"\n\"Free X\"\n"
  it "manages multiple handles" $
    ( runManaged      -- result:  (MonadBaseControl IO m, MonadIO m) => m (),
                      --          unified with IO ()
    $ useHandles )    -- effects: Managed
      `shouldPrint`
        (  "\"Alloc A\"\n"
        ++ "\"Alloc B\"\n"
        ++ "\"Use A\"\n"
        ++ "\"Use B\"\n"
        ++ "\"Free B\"\n"
        ++ "\"Free A\"\n"
        )
  it "manages multiple handles with an error" $
    ( runManaged      -- result:  (MonadBaseControl IO m, MonadIO m) => m (Either String ()),
                      --          unified with IO (Either String ())
    . runError        -- effects: Managed
    $ throwCheck )    -- effects: Error String, Managed
      `shouldPrint`
        (  "\"Alloc A\"\n"
        ++ "\"Alloc B\"\n"
        ++ "\"Use A\"\n"
        ++ "\"Free B\"\n"
        ++ "\"Free A\"\n"
        )