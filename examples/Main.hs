module Main where

-- hspec
import Test.Hspec (Spec, describe, hspec)

import qualified Example.Cont
import qualified Example.Embed
import qualified Example.Error
import qualified Example.FileSystem
import qualified Example.Logger
import qualified Example.Managed
import qualified Example.Map
import qualified Example.Reader
import qualified Example.Resource
import qualified Example.RWS
import qualified Example.State
import qualified Example.Writer

spec :: Spec
spec = do
  describe "Cont"       Example.Cont.spec
  describe "Embed"      Example.Embed.spec
  describe "Error"      Example.Error.spec
  describe "FileSystem" Example.FileSystem.spec
  describe "Logger"     Example.Logger.spec
  describe "Managed"    Example.Managed.spec
  describe "Map"        Example.Map.spec
  describe "Reader"     Example.Reader.spec
  describe "Resource"   Example.Resource.spec
  describe "RWS"        Example.RWS.spec
  describe "State"      Example.State.spec
  describe "Writer"     Example.Writer.spec

main :: IO ()
main = hspec spec