module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign (ForeignError(..)) as F
import JsonPointer (ForeignError(..), JsonPointer(..), JsonPointerElement(..), E, foreignToPtr, jqE, parseJsonPointer, renderJsonPointer)
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
          , pointer: { pointer: JsonPointer [ObjectPointer "a/b"], error: (ForeignError "error") }
          }
        , { foreign: F.ErrorAtProperty "foo" $ F.ErrorAtProperty "bar" (F.ForeignError "error")
          , pointer: { pointer: JsonPointer [ObjectPointer "foo", ObjectPointer "bar"], error: (ForeignError "error") }
          }
        , { foreign: F.ErrorAtProperty "foo" $ F.ErrorAtIndex 0 (F.ForeignError "error")
          , pointer: { pointer: JsonPointer [ObjectPointer "foo", ArrayPointer 0], error: (ForeignError "error") }
          }
        ]
    for_ samples $ \obj ->
      it ("converts " <> (show obj.foreign) <> " to " <> (show obj.pointer)) do
        (foreignToPtr obj.foreign) `shouldEqual` obj.pointer
    let
      jqSamples = [
          { foreign: (Left $ pure $ F.ErrorAtProperty "a/b" (F.ForeignError "error") :: E String)
          , msg: pure "jq .a/b # to display the json at (ForeignError \"error\")"
          }
        , { foreign: (Left $ pure $ F.ErrorAtProperty "foo" $ F.ErrorAtIndex 0 (F.ForeignError "error") :: E String)
          , msg: pure "jq .foo[0] # to display the json at (ForeignError \"error\")"
          }
      ]
    for_ jqSamples $ \obj ->
      it ("converts " <> (show obj.foreign) <> " to " <> (show obj.msg)) do
        (jqE obj.foreign) `shouldEqual` (Left obj.msg)
    
