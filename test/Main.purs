module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_, message, try)
import Elmish ((<?|))
import Elmish.Component (ComponentDef)
import Elmish.Enzyme (at, clickOn, exists, find, findSingle, length, prop, simulate', testComponent, testElement, text, (>>))
import Elmish.Enzyme as Enzyme
import Elmish.Enzyme.Adapter as Adapter
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

foreign import _configureJsDomViaFfi :: Type

main :: Effect Unit
main = do
  Enzyme.configure Adapter.unofficialReact_17
  launchAff_ $ runSpec [specReporter] spec

spec :: Spec Unit
spec = do
  describe "text" $
    it "returns text inside of the current element wrapper" $
      testElement (H.div "" "Foo") $
        text >>= shouldEqual "Foo"

  describe "findSingle" do
    it "gets an element wrapper by a selector" $
      testComponent def $
        findSingle ".bar" >> text >>= shouldEqual "Bar"
    it "crashes when multiple elements are found" $
      testComponent def $
        try (findSingle ".qux p") >>= case _ of
          Left err -> message err `shouldEqual` "Expected a single element matching '.qux p', but found 2"
          Right _ -> fail "Expected findSingle to crash"
    it "crashes when zero elements are found" $
      testComponent def $
        try (findSingle ".qux a") >>= case _ of
          Left err -> message err `shouldEqual` "Expected a single element matching '.qux a', but found 0"
          Right _ -> fail "Expected findSingle to crash"

  describe "find" $
    it "gets multiple elements" $
      testComponent def do
        find ".qux" >> find "p" >> length >>= shouldEqual 2
        find ".qux" >> find "p" >> at 0 >> text >>= shouldEqual "First"

  describe "exists" do
    it "returns `true` when selector is found" $
      testComponent def $
        exists ".bar" >>= shouldEqual true

    it "returns `false` when selector isn’t found" $
      testComponent def $
        exists ".not-found" >>= shouldEqual false

  describe "clickOn" $
    it "simulates a click event on a given selector" $
      testComponent def do
        findSingle ".bar" >> text >>= shouldEqual "Bar"
        clickOn ".toggle-bar"
        exists ".bar" >>= shouldEqual false

  describe "prop" $
    it "returns the value of a given prop" $
      testElement (H.div_ "" { id: "blah" } H.empty) $
        prop "id" >>= shouldEqual "blah"

  describe "simulate'" $
    it "simulates a given event with the given argument on the current wrapper" $
      testComponent def do
        findSingle ".baz" >> do
          prop "value" >>= shouldEqual ""
          simulate' "change" { target: { value: "New text" } }
        findSingle ".baz" >> prop "value" >>= shouldEqual "New text"

-- Test Component

type State =
  { bar :: Boolean
  , baz :: String
  }

data Message
  = ToggleBar
  | ChangeBaz String

def :: ComponentDef Message State
def =
  { init: pure { bar: true, baz: "" }
  , update: \s -> case _ of
      ToggleBar -> pure s { bar = not s.bar }
      ChangeBaz baz -> pure s { baz = baz }
  , view: \s dispatch -> H.fragment
      [ H.div "foo" "Foo"
      , H.div "qux"
        [ H.p "" "First"
        , H.p "" "Second"
        ]
      , if s.bar then
          H.div "bar" "Bar"
        else
          H.empty
      , H.button_ "toggle-bar" { onClick: dispatch ToggleBar } "Hide Bar"
      , H.input_ "baz"
          { onChange: dispatch <?| (map (ChangeBaz <<< _.target.value) <<< toEvent)
          , value: s.baz
          }
      ]
  }
  where
    toEvent :: _ -> _ { target :: { value :: String } }
    toEvent = readForeign
