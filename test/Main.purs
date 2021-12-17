module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish ((<?|))
import Elmish.Component (ComponentDef)
import Elmish.Enzyme (clickOn, exists, find, prop, simulate', testComponent, testElement, text, (>>))
import Elmish.Enzyme as Enzyme
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Foreign (unsafeToForeign)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (runSpec)

foreign import _configureJsDomViaFfi :: Type

main :: Effect Unit
main = do
  Enzyme.configure
  launchAff_ $ runSpec [specReporter] spec

spec :: Spec Unit
spec = do
  describe "text" $
    it "returns text inside of the current element wrapper" $
      testElement (H.div "" "Foo") $
        text >>= shouldEqual "Foo"

  describe "find" $
    it "gets an element wrapper by a selector" $
      testComponent def $
        find ".bar" >> text >>= shouldEqual "Bar"

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
        find ".bar" >> text >>= shouldEqual "Bar"
        clickOn ".toggle-bar"
        exists ".bar" >>= shouldEqual false

  describe "prop" $
    it "returns the value of a given prop" $
      testElement (H.div_ "" { id: "blah" } H.empty) $
        prop "id" >>= shouldEqual "blah"

  describe "simulate'" $
    it "simulates a given event with the given argument on the current wrapper" $
      testComponent def do
        find ".baz" >> do
          prop "value" >>= shouldEqual ""
          simulate' "change" $ unsafeToForeign { target: { value: "New text" } }
        find ".baz" >> prop "value" >>= shouldEqual "New text"

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
