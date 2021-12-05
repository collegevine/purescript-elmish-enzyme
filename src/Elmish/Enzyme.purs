-- | Provides a monadic API for Enzyme with counterparts to each of the
-- | `Test.Enzyme` functions. All of these functons have an implicit
-- | `ElementWrapper` argument. E.g. `Enzyme.find ".t--my-selector" wrapper`
-- | becomes `EnzymeM.find ".t--my-selector"`. This also provides some test
-- | bootstrapping helpers (`testComponent` / `testElement`) which accept a
-- | computation in the `EnzymeM` monad and run it as a test.
module Elmish.Enzyme
  ( EnzymeM
  , testComponent
  , testElement
  , at
  , clickOn
  , debug
  , exists
  , find
  , findSingle
  , is
  , prop
  , setState
  , simulate
  , simulate'
  , simulateCustom'
  , state
  , text
  , count
  , trace
  , update
  , waitUntil
  , waitWhile
  , withElement
  , withElementM
  , withSelector
  , (>>)
  , module ForeignExports
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Elmish (ReactElement)
import Elmish.Component (ComponentDef)
import Foreign (Foreign)
import Elmish.Enzyme.Foreign (ElementWrapper)
import Elmish.Enzyme.Foreign as E
import Elmish.Enzyme.Foreign (configure) as ForeignExports

-- | Monad in which an `ElementWrapper` is implicit in function calls
type EnzymeM = ReaderT ElementWrapper Aff

-- | Runs a test with a `ComponentDef` as the implicit `ElementWrapper`
-- |
-- | ```purescript
-- | it "displays content" do
-- |   EnzymeM.testComponent (MyComponent.def props) do
-- |     EnzymeM.exists ".t--my-content" >>= shouldEqual true
-- | ```
testComponent :: forall m msg state. MonadAff m => ComponentDef msg state -> EnzymeM Unit -> m Unit
testComponent component test = liftAff do
  wrapper <- E.mountComponent component
  runReaderT test wrapper
  E.unmount wrapper

-- | Runs a test with a `ReactElement` as the implicit `ElementWrapper`
-- |
-- | ```purescript
-- | pending' "displays content"
-- |   EnzymeM.testElement (MyElement.render props) do
-- |     EnzymeM.exists ".t--my-content" >>= shouldEqual true
-- | ```
testElement :: forall m. MonadAff m => ReactElement -> EnzymeM Unit -> m Unit
testElement element test = liftAff do
  wrapper <- E.mount element
  runReaderT test wrapper
  E.unmount wrapper

-- | The current context can have multiple nodes. This gets the element at the
-- | given index. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/at.html for more
-- | info.
at :: Int -> EnzymeM ElementWrapper
at index = E.at index =<< ask

-- | Returns the string representing the DOM tree of the current element. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/debug.html for more
-- | info.
debug :: EnzymeM String
debug = E.debug =<< ask

-- | Logs a string representing the DOM tree of the current element.
trace :: EnzymeM Unit
trace = log =<< debug

-- | Returns a `Boolean` indicating whether a given selector exists within the
-- | current element.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/exists.html
-- | for more info.
exists :: String -> EnzymeM Boolean
exists selector = E.exists selector =<< ask

-- | Finds all elements with the given selector within the current element. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/find.html for
-- | more info.
find :: String -> EnzymeM ElementWrapper
find selector = E.find selector =<< ask

-- | Finds a single element with the given selector within the current element
-- | (see also `find`). Crashes if no elements or more than one elements could
-- | be found.
findSingle :: String -> EnzymeM ElementWrapper
findSingle selector = do
  e <- find selector
  when (E.length e /= 1) $
    liftEffect $ throw $ "Expected to find a single element '" <> selector <> "', but found " <> show (E.length e)
  pure e

-- | Returns a `Boolean` indicating whether the current element matches
-- | the given selector.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/is.html
-- | for more info.
is :: String -> EnzymeM Boolean
is selector = E.is selector =<< ask

-- | Returns the value of the current element’s prop with a certain key.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/prop.html
-- | for more info.
prop :: forall a. String -> EnzymeM a
prop key = E.prop key =<< ask

-- | Sets the state of the given `ElementWrapper`. This is asynchronous, so runs
-- | in a `MonadAff`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/setState.html
-- | for more info.
setState :: forall state. state -> EnzymeM Unit
setState newState =
  E.setState newState =<< ask

-- | A convenience function for calling `simulate'` without an `event` arg.
simulate :: String -> EnzymeM Unit
simulate eventType =
  E.simulate eventType =<< ask

-- | Simulates a certain event type on the current element. The event
-- | argument is passed to the component’s event handler.
-- |
-- | NOTE: This doesn’t actually emit an event, but just triggers the event
-- | handler on the wrapper.
-- |
-- | NOTE 2: This function only works for native HTML elements. For emitting
-- | events on custom React components, use `simulateCustom`.
-- |
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/simulate.html
-- | for more info.
simulate' :: forall a. String -> a -> EnzymeM Unit
simulate' eventType event =
  E.simulate' eventType event =<< ask

-- | Simulates an event on a custom React component (i.e. not an HTML element).
-- | For reasons to complicated to discuss here, the regular `simulate` doesn't
-- | work on custom components, so we provide this workaround.
simulateCustom' :: forall a. String -> a -> EnzymeM Unit
simulateCustom' eventType event =
  E.simulateCustom' eventType event =<< ask

-- | A convienience shorthand for clicking an element known by CSS selector.
-- | Just a wrapper around `withSelector` and `simulate "click"`.
clickOn :: String -> EnzymeM Unit
clickOn selector = withSelector selector $ simulate "click"

-- | Returns the state of the current element. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/state.html for more
-- | info.
state :: EnzymeM Foreign
state = E.state =<< ask

-- | Returns the text within the current element.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/text.html
-- | for more info.
text :: EnzymeM String
text = E.text =<< ask

-- | Returns the number of times a given selector appears.
count :: String -> EnzymeM Int
count selector = E.count selector =<< ask

-- | Updates the current element to reflect the latest state. Call this function
-- | whenever you think there could be an async change of state that caused a
-- | re-render. For some reason, Enzyme won't pick up the changes automatically.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/update.html for
-- | more info.
update :: EnzymeM Unit
update =
  E.update =<< ask

-- | Takes an `ElementWrapper` and runs an `EnzymeM` computation with the given
-- | wrapper as the new implicit wrapper. This can be thought of as analogous to
-- | Capybara’s `within`.
-- |
-- | ```purescript
-- | EnzymeM.withElement button do
-- |   EnzymeM.simulate "click"
-- |   EnzymeM.prop "disabled" >>= shouldEqual true
-- | ```
withElement :: forall a. ElementWrapper -> EnzymeM a -> EnzymeM a
withElement wrapper =
  withReaderT $ const wrapper

-- | Monadic version of `withElement` which allows chaining `EnzymeM` functions.
-- | The intent is to make it so that there’s no need to reach into `Enzyme`, so
-- | that developers don’t need to remember when to use `EnzymeM` vs. `Enzyme`.
-- |
-- | Meant to be used with the infix operator (`>>`):
-- |
-- | ```purescript
-- | EM.find ".foo" >> EM.at 1 >> EM.find ".bar" >> EM.simulate "click"
-- | ```
-- |
-- | instead of having to use `Enzyme` functions also (note the `E.*`s instead
-- | of `EM.*`).
-- |
-- | ```purescript
-- | EM.find ".foo" >>= E.at 1 >>= E.find ".bar" >>= E.simulate "click"
-- | ```
withElementM :: forall a. EnzymeM ElementWrapper -> EnzymeM a -> EnzymeM a
withElementM el f = el >>= \e -> withElement e f

infixl 1 withElementM as >>

-- | A convenience function which finds an element for the given selector and
-- | passes it to `withElement`.
-- |
-- | ```purescript
-- | EnzymeM.withSelector ".t--my-button" do
-- |   EnzymeM.simulate "click"
-- |   EnzymeM.prop "disabled" >>= shouldEqual true
-- | ```
withSelector :: forall a. String -> EnzymeM a -> EnzymeM a
withSelector selector m = do
  wrapper <- findSingle selector
  withElement wrapper m

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after a second.
waitWhile :: EnzymeM Boolean -> EnzymeM Unit
waitWhile f = waitUntil $ not <$> f

-- | Performs active wait while the given condition is false. Times out with a
-- | crash after a second.
waitUntil :: EnzymeM Boolean -> EnzymeM Unit
waitUntil f = go 1000.0
  where
    go remaining = do
      when (remaining <= 0.0) $
        liftEffect $ throw "Timeout expired"
      liftAff $ delay $ Milliseconds 1.0
      update
      unlessM f $ go $ remaining - 1.0
