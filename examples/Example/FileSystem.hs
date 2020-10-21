{-# LANGUAGE TemplateHaskell #-}
-- | Example of a custom effect for file systems access.
module Example.FileSystem where

-- base
import Data.Coerce    (coerce)
import Data.Maybe     (fromMaybe)
import Prelude hiding (lookup, readFile, writeFile)
import qualified System.IO as IO

-- hspec
import Test.Hspec (Spec, it, shouldBe)

-- effet
import Control.Effect.Identity
import Control.Effect.Machinery
import Control.Effect.Map
import Control.Effect.Map.Strict

import Example.Logger
import Hspec (shouldPrint)

--- Custom Effects -------------------------------------------------------------

-- | An effect for representing file system access.
class Monad m => FileSystem m where
  readFile  :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

makeEffect ''FileSystem

--- Effect Interpretations -----------------------------------------------------

-- | An effect handler for 'FileSystem' which touches the actual file system.
newtype LocalFS m a =
  LocalFS { runLocalFS :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance MonadIO m => FileSystem (LocalFS m) where
  readFile path = liftIO $ IO.readFile path
  writeFile path txt = liftIO $ IO.writeFile path txt

runLocalFileSystem :: (FileSystem `Via` LocalFS) m a -> m a
runLocalFileSystem = coerce

-- | An effect handler for 'FileSystem' which provides a virtual file system.
newtype VirtualFS m a =
  VirtualFS { runVirtualFS :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance Map FilePath String m => FileSystem (VirtualFS m) where
  readFile path = VirtualFS $ fromMaybe "" <$> lookup path
  writeFile path txt = VirtualFS $ insert path txt

runVirtualFileSystem :: (FileSystem `Via` VirtualFS) m a -> m a
runVirtualFileSystem = coerce

-- | An effect handler for 'FileSystem' which intercepts the access to files
-- and logs the actions using the 'Logger' effect.
newtype Interceptor m a =
  Interceptor { unInterceptor :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via IdentityT
    deriving (MonadBase b, MonadBaseControl b)

instance (FileSystem m, Logger m) => FileSystem (Interceptor m) where
  readFile path = Interceptor $
    logStr ("READ: " ++ path) >> readFile path
  writeFile path txt = Interceptor $
    logStr ("WRITE: " ++ path ++ " [" ++ txt ++ "]") >> writeFile path txt

runInterceptor :: (FileSystem `Via` Interceptor) m a -> m a
runInterceptor = coerce

--- Example Programs -----------------------------------------------------------

-- | Simple program which writes a file and reads it afterwards and returns the
-- old and new file content.
writeRead :: FileSystem m => FilePath -> String -> m (String, String)
writeRead path txt = do
  old <- readFile path
  writeFile path txt
  new <- readFile path
  pure (old, new)

-- | Appends a text to a file given by a specific file path, and returns the old
-- and new lengths of the file.
append :: FileSystem m => FilePath -> String -> m (Int, Int)
append path txt = do
  content <- readFile path
  let size = length content
  seq size $ writeFile path (content ++ txt)
  pure (size, size + length txt)

--- Test Cases -----------------------------------------------------------------

spec :: Spec
spec = do
  it "evaluates writeRead virtually" $
    ( runIdentity             -- result:  (String, String)
    . runMap                  -- result:  Monad m => m (String, String)
    . runVirtualFileSystem    -- effects: Map FilePath String
    $ writeRead "tmp" "hello" -- effects: FileSystem
    ) `shouldBe` ("", "hello")
  it "evaluates append virtually" $
    ( runIdentity             -- result:  (Int, Int)
    . runMap                  -- result:  Monad m => m (Int, Int)
    . runVirtualFileSystem    -- effects: Map FilePath String
    $ append "tmp" "hello"    -- effects: FileSystem
    ) `shouldBe` (0, 5)
  it "intercepts append by printing" $
    ( runPrintLogger          -- result:  MonadIO m => m (), unified with IO ()
    . runMap                  -- effects: Logger
    . runVirtualFileSystem    -- effects: Map FilePath String, Logger
    . runInterceptor          -- effects: FileSystem, Logger
    $ append "tmp" "hello"    -- effects: FileSystem
    ) `shouldPrint` "\"READ: tmp\"\n\"WRITE: tmp [hello]\"\n"
  it "intercepts append by collecting" $
    ( runIdentity             -- result:  ((Int, Int), [String])
    . runCollectLogger        -- result:  Monad m => m ((Int, Int), [String])
    . runMap                  -- effects: Logger
    . runVirtualFileSystem    -- effects: Map FilePath String, Logger
    . runInterceptor          -- effects: FileSystem, Logger
    $ append "tmp" "hello"    -- effects: FileSystem
    ) `shouldBe` ((0, 5), ["WRITE: tmp [hello]", "READ: tmp"])