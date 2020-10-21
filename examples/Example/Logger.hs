{-# LANGUAGE TemplateHaskell #-}
-- | Example of a custom effect for logging.
module Example.Logger where

-- base
import Data.Coerce    (coerce)
import Prelude hiding (print)

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.Machinery
import Control.Effect.Reader

-- transformers
import qualified Control.Monad.Trans.State.Strict as S

import Hspec (print, shouldPrint)

--- Custom Effects -------------------------------------------------------------

-- | An effect for logging.
class Monad m => Logger m where
  logStr :: String -> m ()

makeEffect ''Logger

--- Effect Interpretations -----------------------------------------------------

-- | An effect handler for 'Logger' which prints the logged strings.
newtype PrintLogger m a =
  PrintLogger { unPrintLogger :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance MonadIO m => Logger (PrintLogger m) where
  logStr = print

runPrintLogger :: (Logger `Via` PrintLogger) m a -> m a
runPrintLogger = coerce

-- | An effect handler for 'Logger' which collects the logged strings in reverse
-- order (newest comes first).
newtype CollectLogger m a =
  CollectLogger { unCollectLogger :: S.StateT [String] m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl)
    deriving (MonadBase b, MonadBaseControl b)

instance Monad m => Logger (CollectLogger m) where
  logStr txt = CollectLogger $ S.modify (txt :)

runCollectLogger :: (Logger `Via` CollectLogger) m a -> m (a, [String])
runCollectLogger = flip S.runStateT [] . coerce

-- | An effect handler for 'Logger' which suppresses logging.
newtype NoLogger m a =
  NoLogger { unNoLogger :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance Monad m => Logger (NoLogger m) where
  logStr _ = pure ()

runNoLogger :: (Logger `Via` NoLogger) m a -> m a
runNoLogger = coerce

--- Example Programs -----------------------------------------------------------

-- | Simple program which logs some steps.
logging :: (Logger m, MonadIO m) => m ()
logging = do
  print "A"
  logStr $ "Done A"
  print "B"
  logStr $ "Done B"
  print "C"
  logStr $ "Done C"

-- | Logs and returns the sum of a number and the reader value.
adding :: (Reader Int m, Logger m) => Int -> m Int
adding offset = do
  num <- ask
  logStr $ "Asked: " ++ show num
  let result = offset + num
  logStr $ "Sum: " ++ show result
  pure result

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates logging by suppression" $
    ( runNoLogger      -- result:  MonadIO m => m (), unified with IO ()
    $ logging          -- effects: Logger, MonadIO
    ) `shouldPrint`
        "\"A\"\n\"B\"\n\"C\"\n"
  it "evaluates logging by printing" $
    ( runPrintLogger   -- result:  MonadIO m => m (), unified with IO ()
    $ logging          -- effects: Logger, MonadIO
    ) `shouldPrint`
        "\"A\"\n\"Done A\"\n\"B\"\n\"Done B\"\n\"C\"\n\"Done C\"\n"
  it "evaluates logging by collecting" $
    ( runIdentity      -- result:  (Int, [String])
    . runCollectLogger -- result:  Monad m => m (Int, [String])
    . runReader 1327   -- effects: Logger
    $ adding 10        -- effects: Reader Int, Logger
    ) `shouldBe`
        (1337, ["Sum: 1337", "Asked: 1327"])