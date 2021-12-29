-- | Bindings for the [Enzyme library](https://enzymejs.github.io/enzyme/) to
-- | use with the [Elmish
-- | library](https://github.com/collegevine/purescript-elmish).
-- |
-- | The API here is presented in monadic style, with the idea that there is
-- | always a "current" DOM element (or, more precisely, current
-- | `ElementWrapper`, which may refer to zero or more actual DOM elements),
-- | with respect to which all operations run. A test starts with either
-- | `testComponent` or `testElement`, and it's that component (or element) that
-- | becomes the "current" element at the start, for example:
-- |
-- |```purs
-- |-- The main module:
-- |myComponent :: ComponentDef Message State
-- |myComponent = { init, view, update } where ...
-- |
-- |-- The test module:
-- |import Elmish.Enzyme (testComponent, text)
-- |
-- |someTest = it "contains the right text" do
-- |  testComponent myComponent do
-- |    content <- text
-- |    content `shouldContain` "right"
-- |```
-- |
-- | From there, it's possible to "drill down" into a particular element, making
-- | it "current" temporarily, via `withSelector` or `withElement` (similar to
-- | Capybara's `within`), for example:
-- |
-- |```purs
-- |import Elmish.Enzyme (testComponent, text, withSelector)
-- |
-- |someTest = it "has two buttons with the right text" do
-- |  testComponent myComponent do
-- |    withSelector "button.first-button" do
-- |      content <- text
-- |      content `shouldEqual` "I'm the first button!"
-- |
-- |    withSelector "button.second-button" do
-- |      content <- text
-- |      content `shouldEqual` "I'm the second button!"
-- |```
-- |
-- | Alternatively, operations that return an `ElementWrapper` may be combined
-- | in a chain using the `>>` operator (which is just a convenient facade for
-- | `withElement` under the hood):
-- |
-- |```purs
-- |import Elmish.Enzyme (testComponent, text, find, at, (>>))
-- |
-- |someTest = it "has two buttons with the right text" do
-- |  testComponent myComponent do
-- |    -- A short chain, just two functions:
-- |    b1 <- find "button.first-button" >> text
-- |    b1 `shouldEqual` "I'm the first button!"
-- |
-- |    -- A longer chain: `find`, then `at`, then `text`
-- |    b2 <- find "button" >> at 1 >> text
-- |    b2 `shouldEqual` "I'm the second button!"
-- |
-- |    -- An even longer chain:
-- |    b3 <- find ".card" >> at 3 >> find "button" >> at 1 >> text
-- |    b3 `shouldEqual` "I'm second button in 4th card!"
-- |```
-- |
-- | The end result is an API that somewhat resembles how Emzyme would be used
-- | from JavaScript, as well as other similar testing libraries, sych as
-- | Capybara.
module Elmish.Enzyme
  ( EnzymeM
  , testComponent, testElement
  , at
  , clickOn
  , count
  , debug
  , exists
  , find
  , findSingle
  , forEach
  , is
  , length
  , mapEach
  , name
  , parent
  , prop
  , simulate, simulate', simulateCustom'
  , state
  , text
  , toArray
  , trace
  , unsafeSetState
  , update
  , waitUntil, waitUntil'
  , waitWhile, waitWhile'
  , withElement, withElementM
  , withSelector
  , (>>)
  , module ForeignExports
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Data.Traversable (traverse)
import Debug (class DebugWarning)
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
import Elmish.Enzyme.Foreign (ElementWrapper, configure) as ForeignExports

-- | Monad for running Enzyme tests. Keeps a reference to the "current" DOM
-- | element(s).
type EnzymeM = ReaderT ElementWrapper Aff

-- | Runs a test with a `ComponentDef` as the implicit `ElementWrapper`
-- |
-- | ```purescript
-- | it "displays content" do
-- |   testComponent (MyComponent.def props) do
-- |     exists ".t--my-content" >>= shouldEqual true
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
-- |   testElement (MyElement.render props) do
-- |     exists ".t--my-content" >>= shouldEqual true
-- | ```
testElement :: forall m. MonadAff m => ReactElement -> EnzymeM Unit -> m Unit
testElement element test = liftAff do
  wrapper <- E.mount element
  runReaderT test wrapper
  E.unmount wrapper

-- | The current context can contain multiple DOM elements. This gets the
-- | element at the given index (zero-based). See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/at.html for more
-- | info.
-- |
-- |```purs
-- |find "button" >> at 3 >> text >>= shouldEqual "Fourth button"
-- |```
at :: Int -> EnzymeM ElementWrapper
at index = E.at index =<< ask

-- | Returns the string representing the DOM tree of the current element(s). See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/debug.html for more
-- | info.
debug :: DebugWarning => EnzymeM String
debug = E.debug =<< ask

-- | Logs a string representing the DOM tree of the current element(s).
trace :: DebugWarning => EnzymeM Unit
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

-- | Returns parent of the current element. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/find.html for
-- | more info.
parent :: EnzymeM ElementWrapper
parent = E.parent =<< ask

-- | Finds a single element with the given selector within the current element
-- | (see also `find`). Crashes if no elements or more than one element was
-- | found.
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

-- | Sets the state of the current element. Note that this is
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/setState.html
-- | for more info.
-- |
-- | NOTE: this is a type-unsafe operation. There is no check to make sure the
-- | state being set here has the type of the actual state the component in
-- | question is using.
unsafeSetState :: forall state. state -> EnzymeM Unit
unsafeSetState newState =
  E.unsafeSetState newState =<< ask

-- | A convenience function for calling `simulate'` without an `event` arg.
simulate :: String -> EnzymeM Unit
simulate eventType =
  E.simulate eventType =<< ask

-- | Simulates a certain event type on the current element. The event argument
-- | is a record that gets merged with a simulated React synthetic event before
-- | being passed to the component’s event handler. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/simulate.html for
-- | more info.

-- | NOTE: This doesn’t actually emit an event, but just triggers the event
-- | handler on the wrapper.
-- |
-- | NOTE 2: This function only works for native HTML elements. For emitting
-- | events on custom React components, use `simulateCustom`.
simulate' :: forall r. String -> Record r -> EnzymeM Unit
simulate' eventType event =
  E.simulate' eventType event =<< ask

-- | Simulates an event on a custom React component (i.e. not an HTML element).
-- | For reasons to complicated to discuss here, the regular `simulate` doesn't
-- | work on custom components, so we provide this workaround.
-- |
-- | NOTE: the second parameter is passed to the event handler without any
-- | checks whatsoever. This is, of course, not type-safe, but it is in line
-- | with what the event handler should expect anyway: after all, the underlying
-- | JavaScript component may pass anything at all as event argument.
simulateCustom' :: forall a. String -> a -> EnzymeM Unit
simulateCustom' eventType event =
  E.simulateCustom' eventType event =<< ask

-- | A convienience shorthand for clicking an element known by CSS selector
clickOn :: String -> EnzymeM Unit
clickOn selector = find selector >> simulate "click"

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

-- | Returns tag name of the current element.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/name.html
-- | for more info.
name :: EnzymeM String
name = E.name =<< ask

-- | Returns the number of times a given selector appears.
count :: String -> EnzymeM Int
count selector = E.count selector =<< ask

-- | Updates the current element to reflect the latest state. Call this function
-- | whenever you think there could be an async change of state that caused a
-- | re-render. For some reason, Enzyme won't pick up the changes automatically.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/update.html for
-- | more info.
-- |
-- | NOTE: this only works on the "root" element, which means it cannot be
-- | called inside `withSelector` or `withElement`.
update :: EnzymeM Unit
update = E.update =<< ask

-- | Takes an `ElementWrapper` and runs an `EnzymeM` computation with the given
-- | wrapper as the new implicit wrapper. This can be thought of as analogous to
-- | Capybara’s `within`.
-- |
-- | ```purescript
-- | button <- find "button.my-button"
-- | withElement button do
-- |   simulate "click"
-- |   prop "disabled" >>= shouldEqual true
-- | ```
withElement :: forall a. ElementWrapper -> EnzymeM a -> EnzymeM a
withElement wrapper =
  withReaderT $ const wrapper

-- | A version of `withElement` that takes the `ElementWrapper` wrapped in
-- | `EnzymeM` rather than "naked". Aliased as the `>>` operator, this allows
-- | for handy chaining of operations that return an `ElementWrapper`, for
-- | example:
-- |
-- | ```purescript
-- | find ".foo" >> at 1 >> find ".bar" >> simulate "click"
-- | ```
withElementM :: forall a. EnzymeM ElementWrapper -> EnzymeM a -> EnzymeM a
withElementM el f = el >>= \e -> withElement e f

infixl 1 withElementM as >>

-- | Basically, a DSL-friendly equivalent of `map`: for each element in the
-- | current `ElementWrapper` performs the given action and returns all results
-- | of that action as an array.
-- |
-- | Example:
-- |
-- |     allNames <- find ".t--foo" >> mapEach text
-- |     allValues <- find ".t--foo" >> mapEach (prop "value")
-- |
mapEach :: forall a. EnzymeM a -> EnzymeM (Array a)
mapEach f = toArray >>= traverse \e -> withElement e f

-- | Returns all elements contained in the current `ElementWrapper` as an array.
toArray :: EnzymeM (Array ElementWrapper)
toArray = E.toArray =<< ask

-- | Basically, a DSL-friendly equivalent of `for_`: for each element in the
-- | current `ElementWrapper` performs the given effect.
-- |
-- | Example:
-- |
-- |     find "button" >> forEach (simulate "click")
-- |
-- |     find ".t--foo" >> forEach do
-- |       find "input[type=text]" >> simulate' "change" { target: { value: "new text" } }
-- |       find "input[type=checkbox]" >> simulate "change"
-- |       find ".t--bar" >> text >>= shouldEqual "qux"
-- |
forEach :: EnzymeM Unit -> EnzymeM Unit
forEach f = E.forEach (\e -> withElement e f) =<< ask

-- | Returns number of elements in a given wrapper
length :: EnzymeM Int
length = E.length <$> ask

-- | A convenience function which finds an element for the given selector and
-- | passes it to `withElement`.
-- |
-- | ```purescript
-- | withSelector ".t--my-button" do
-- |   simulate "click"
-- |   prop "disabled" >>= shouldEqual true
-- | ```
withSelector :: forall a. String -> EnzymeM a -> EnzymeM a
withSelector selector m = do
  wrapper <- findSingle selector
  withElement wrapper m

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after a second.
waitWhile :: EnzymeM Boolean -> EnzymeM Unit
waitWhile = waitWhile' (Milliseconds 1000.0)

-- | Performs active wait while the given condition is false. Times out with a
-- | crash after a second.
waitUntil :: EnzymeM Boolean -> EnzymeM Unit
waitUntil = waitUntil' (Milliseconds 1000.0)

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after given time period.
waitWhile' :: Milliseconds -> EnzymeM Boolean -> EnzymeM Unit
waitWhile' timeout f = waitUntil' timeout $ not <$> f

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after given time period.
waitUntil' :: Milliseconds -> EnzymeM Boolean -> EnzymeM Unit
waitUntil' (Milliseconds timeout) f = go timeout
  where
    go remaining = do
      when (remaining <= 0.0) $
        liftEffect $ throw "Timeout expired"
      liftAff $ delay $ Milliseconds 1.0
      update
      unlessM f $ go $ remaining - 1.0
