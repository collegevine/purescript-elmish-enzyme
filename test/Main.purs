module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish.Component (ComponentDef)
import Elmish.Enzyme (exists, find, simulate, testComponent, text, (>>))
import Elmish.Enzyme as Enzyme
import Elmish.HTML.Styled as H
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
spec =
  describe "Foo" do
    it "displays text" $
      testComponent def $
        find ".foo" >> text >>= shouldEqual "Foo"
    it "allows toggling “Bar”" $
      testComponent def do
        find ".bar" >> text >>= shouldEqual "Bar"
        find ".toggle-bar" >> simulate "click"
        exists ".bar" >>= shouldEqual false

-- Test Component

type State =
  { bar :: Boolean
  }

data Message
  = ToggleBar

def :: ComponentDef Message State
def =
  { init: pure { bar: true }
  , update: \s -> case _ of
      ToggleBar -> pure s { bar = not s.bar }
  , view: \s dispatch -> H.fragment
      [ H.div "foo" "Foo"
      , if s.bar then
          H.div "bar" "Bar"
        else
          H.empty
      , H.button_ "toggle-bar" { onClick: dispatch ToggleBar } "Hide Bar"
      ]
  }
