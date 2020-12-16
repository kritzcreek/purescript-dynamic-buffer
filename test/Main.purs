module Test.Main where

import Prelude

import Data.ArrayBuffer.Types (ArrayView, Uint8)
import DynamicBuffer (DBuffer)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe)
import Test.Spec as Spec
import Test.Spec.Assertions (expectError, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

it :: String -> Effect Unit -> Spec.Spec Unit
it msg = Spec.it msg <<< liftEffect

foreign import arrayViewToArray :: ArrayView Uint8 -> Array Int

bufferEquals :: DBuffer -> Array Int -> Effect Unit
bufferEquals actual expected = do
  contents <- DBuffer.contents actual
  arrayViewToArray contents `shouldEqual` expected

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "dynamic-buffer" do
    it "initializes an empty buffer" do
      b <- DBuffer.create 1
      b `bufferEquals` []
    it "adds a byte to a buffer" do
      b <- DBuffer.create 1
      DBuffer.addByte b 1
      b `bufferEquals` [1]
    it "fails to add an out-of-range value" do
      b <- DBuffer.create 1
      expectError (DBuffer.addByte b 256)
    it "resizes beyond its initial capacity" do
      b <- DBuffer.create 1
      DBuffer.addByte b 1
      DBuffer.addByte b 2
      DBuffer.addByte b 3
      b `bufferEquals` [1, 2, 3]
    it "returns the current buffers size" do
      b <- DBuffer.create 1
      s1 <- DBuffer.size b
      s1 `shouldEqual` 0
      DBuffer.addByte b 1
      s2 <- DBuffer.size b
      s2 `shouldEqual` 1
      DBuffer.addByte b 2
      s3 <- DBuffer.size b
      s3 `shouldEqual` 2
    describe "addBuffer" do
      it "adds buffers to buffers" do
        b1 <- DBuffer.create 1
        b2 <- DBuffer.create 2
        DBuffer.addByte b1 1
        DBuffer.addByte b2 2
        DBuffer.addByte b2 3
        DBuffer.addBuffer b1 b2
        b1 `bufferEquals` [1, 2, 3]
      it "adds itself" do
        b <- DBuffer.create 8
        DBuffer.addByte b 1
        DBuffer.addByte b 2
        DBuffer.addByte b 3
        DBuffer.addBuffer b b
        b `bufferEquals` [1, 2, 3, 1, 2, 3]
    describe "setByte" do
      it "overrides already written bytes" do
        b <- DBuffer.create 1
        DBuffer.addByte b 1
        DBuffer.addByte b 1
        DBuffer.addByte b 3
        b `bufferEquals` [1, 1, 3]
        DBuffer.setByte b 1 2
        b `bufferEquals` [1, 2, 3]
      it "fails to override bytes out of bounds" do
        b <- DBuffer.create 1
        DBuffer.addByte b 1
        b `bufferEquals` [1]
        expectError (DBuffer.setByte b 1 2)
    describe "addUtf8" do
      it "creates buffers from utf8 encoded text (ASCII)" do
        b <- DBuffer.fromUtf8 "abc"
        b `bufferEquals` [97, 98, 99]
      it "creates buffers from utf8 encoded text (Unicode)" do
        b <- DBuffer.fromUtf8 "😃"
        b `bufferEquals` [240, 159, 152, 131]
