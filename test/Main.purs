module Test.Main where

import Prelude

import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign (ForeignError(..)) as F
import JsonPointer (ForeignError(..), JsonPointer(..), foreignToPtr, parseJsonPointer, renderJsonPointer)
import Test.QuickCheck ((===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "JSONPointer" do
    it "roundtrips" $ quickCheck \s -> (renderJsonPointer $ parseJsonPointer s) === s
    let
      samples = [
          { foreign: F.ErrorAtProperty "a/b" (F.ForeignError "error")
          , pointer: { pointer: JsonPointer ["a/b"], error: (ForeignError "error") }
          }
        , { foreign: F.ErrorAtProperty "foo" $ F.ErrorAtProperty "bar" (F.ForeignError "error")
          , pointer: { pointer: JsonPointer ["foo", "bar"], error: (ForeignError "error") }
          }
        , { foreign: F.ErrorAtProperty "foo" $ F.ErrorAtIndex 0 (F.ForeignError "error")
          , pointer: { pointer: JsonPointer ["foo", "0"], error: (ForeignError "error") }
          }
        ]
    for_ samples $ \obj ->
      it ("converts " <> (show obj.foreign) <> " to " <> (show obj.pointer)) do
        (foreignToPtr obj.foreign) `shouldEqual` obj.pointer
