module Hspec (print, shouldPrint) where

-- base
import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Prelude         hiding (print)
import System.IO.Unsafe       (unsafePerformIO)

-- hspec
import Test.Hspec (shouldBe)

stdoutMock :: IORef String
stdoutMock = unsafePerformIO (newIORef "")

print :: (MonadIO m, Show a) => a -> m ()
print txt = liftIO $
  modifyIORef stdoutMock (++ show txt ++ "\n")

shouldPrint :: IO a -> String -> IO ()
shouldPrint action expectedOutput =
  bracket
    ( action )
    ( \_ -> writeIORef stdoutMock "" )
    ( \_ -> readIORef stdoutMock >>= (`shouldBe` expectedOutput) )