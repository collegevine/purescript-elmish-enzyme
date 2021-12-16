-- | Provides an API for using [Enzyme](https://enzymejs.github.io/enzyme/).
-- | This is a pretty thin wrapper around Enzyme with a few convenience
-- | functions added.
module Elmish.Enzyme.Foreign
  ( ElementWrapper
  , configure
  , count
  , mount
  , mountComponent
  , shallow
  , shallowComponent
  , at
  , debug
  , exists
  , find
  , forEach
  , is
  , length
  , name
  , parent
  , prop
  , setState
  , simulate
  , simulate'
  , simulateCustom'
  , state
  , text
  , toArray
  , unmount
  , update
  ) where

import Prelude

import Data.Array ((..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Nullable as Nullable
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFn2, EffectFn3, EffectFnAff, fromEffectFnAff, runEffectFn2, runEffectFn3)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Elmish (ComponentDef, ReactElement, construct)
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Foreign (Foreign, unsafeToForeign)

-- | Configures the correct Enzyme adapter. Called once in the main spec.
configure :: Effect Unit
configure =
  runEffectFn1 configure_ unit

-- | Fully mounts a `ReactElement` and returns an `ElementWrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/shallow.html for more info.
mount :: forall m. MonadEffect m => ReactElement -> m ElementWrapper
mount =
  liftEffect <<< runEffectFn1 mount_

-- | A convenience function for creating a full wrapper from a `ComponentDef`
-- | rather than a `ReactElement`. This also adds a `delay (Milliseconds 0.0)`
-- | to allow any `Effect`s in `def.init` to run.
mountComponent :: forall m msg state. MonadAff m => ComponentDef msg state -> m ElementWrapper
mountComponent def = do
  wrapper <- liftEffect $ mount =<< construct def
  liftAff $ delay (Milliseconds 0.0)
  update wrapper
  pure wrapper

-- | Creates a “shallow wrapper” around an element, which basically means the
-- | DOM tree is rendered up to the next layer of React components. So all plain
-- | HTML within the given `ReactElement` is rendered, but not HTML within
-- | nested components. This does not cause a headless browser to spin up.
-- | See https://enzymejs.github.io/enzyme/docs/api/shallow.html for more info.
shallow :: forall m. MonadEffect m => ReactElement -> m ElementWrapper
shallow =
  liftEffect <<< runEffectFn1 shallow_

-- | A convenience function for creating a shallow wrapper from a `ComponentDef`
-- | rather than a `ReactElement`. This also adds a `delay (Milliseconds 0.0)`
-- | to allow any `Effect`s in `def.init` to run.
shallowComponent :: forall m msg state. MonadAff m => ComponentDef msg state -> m ElementWrapper
shallowComponent def = do
  wrapper <- liftEffect $ shallow $
    wrapWithLocalState (ComponentName "Shallow Test Component") (const def) {}
  liftAff $ delay (Milliseconds 0.0)
  pure wrapper

-- | An `ElementWrapper` can have multiple nodes. This gets the node at the
-- | given index.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/at.html for
-- | more info.
at :: forall m. MonadEffect m => Int -> ElementWrapper -> m ElementWrapper
at idx w = liftEffect $ runEffectFn2 at_ idx w

-- | Returns all elements contained in the given `ElementWrapper` as an array.
toArray :: forall m. MonadEffect m => ElementWrapper -> m (Array ElementWrapper)
toArray e = for (0 .. (length e - 1)) \idx -> at idx e

-- | Runs the given action for every element contained in the given
-- | `ElementWrapper` as an array.
forEach :: forall m. MonadEffect m => (ElementWrapper -> m Unit) -> ElementWrapper -> m Unit
forEach f e = for_ (0 .. (length e - 1)) \idx -> at idx e >>= f

-- | Prints the shallow DOM tree up to any nested React components.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/debug.html
-- | for more info.
debug :: forall m. MonadEffect m => ElementWrapper -> m String
debug = liftEffect <<< runEffectFn1 debug_

-- | Returns a `Boolean` indicating whether a given selector exists within the
-- | given `ElementWrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/exists.html
-- | for more info.
exists :: forall m. MonadEffect m => String -> ElementWrapper -> m Boolean
exists selector w = liftEffect $ runEffectFn2 exists_ selector w

-- | Finds all elements with the given selector within the given
-- | `ElementWrapper`. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/find.html for
-- | more info.
find :: forall m. MonadEffect m => String -> ElementWrapper -> m ElementWrapper
find selector w = liftEffect $ runEffectFn2 find_ selector w

-- | Returns parent of the given `ElementWrapper`. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/parent.html for
-- | more info.
parent :: forall m. MonadEffect m => ElementWrapper -> m ElementWrapper
parent w = liftEffect $ runEffectFn1 parent_ w

-- | Returns a `Boolean` indicating whether the given `ElementWrapper` matches
-- | the given selector.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/is.html
-- | for more info.
is :: forall m. MonadEffect m => String -> ElementWrapper -> m Boolean
is selector w = liftEffect $ runEffectFn2 is_ selector w

-- | Returns number of elements in a given wrapper
foreign import length :: ElementWrapper -> Int

-- | Returns the value of an `ElementWrapper`’s prop with a certain key.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/prop.html
-- | for more info.
prop :: forall a m. MonadEffect m => String -> ElementWrapper -> m a
prop selector w = liftEffect $ runEffectFn2 prop_ selector w

-- | Sets the state of the given `ElementWrapper`. This is asynchronous, so runs
-- | in a `MonadAff`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/setState.html
-- | for more info.
setState :: forall state m. MonadAff m => state -> ElementWrapper -> m Unit
setState newState =
  liftAff <<< fromEffectFnAff <<< runFn2 setState_ newState

-- | A convenience function for calling `simulate'` without an `event` arg.
simulate :: forall m. MonadEffect m => String -> ElementWrapper -> m Unit
simulate eventType =
  simulate' eventType Nullable.null

-- | Simulates a certain event type on a given `ElementWrapper`. The event
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
simulate' :: forall m a. MonadEffect m => String -> a -> ElementWrapper -> m Unit
simulate' eventType event =
  liftEffect <<< runEffectFn3 simulate_ eventType (unsafeToForeign event)

-- | Simulates an event on a custom React component (i.e. not an HTML element).
-- | For reasons to complicated to discuss here, the regular `simulate` doesn't
-- | work on custom components, so we provide this workaround.
simulateCustom' :: forall m a. MonadEffect m => String -> a -> ElementWrapper -> m Unit
simulateCustom' eventType event =
  liftEffect <<< runEffectFn3 simulateCustom_ eventType (unsafeToForeign event)

-- | Returns the state of the given `ElementWrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/state.html
-- | for more info.
state :: forall m. MonadEffect m => ElementWrapper -> m Foreign
state = liftEffect <<< runEffectFn1 state_

-- | Returns the text within the given `ElementWrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/text.html
-- | for more info.
text :: forall m. MonadEffect m => ElementWrapper -> m String
text = liftEffect <<< runEffectFn1 text_

-- | Returns tag name of the given `ElementWrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/name.html
-- | for more info.
name :: forall m. MonadEffect m => ElementWrapper -> m String
name = liftEffect <<< runEffectFn1 name_

-- | Returns the number of times a given selector appears within a wrapper.
count :: forall m. MonadEffect m => String -> ElementWrapper -> m Int
count selector wrapper = do
  length <$> find selector wrapper

-- | Unmounts a fully mounted component.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/unmount.html
-- | for more info.
unmount :: forall m. MonadEffect m => ElementWrapper -> m Unit
unmount =
  liftEffect <<< runEffectFn1 unmount_

-- | Updates the given `ElementWrapper` to reflect the latest state. Call this
-- | function whenever you think there could be an async change of state that
-- | caused a re-render. For some reason, Enzyme won't pick up the changes
-- | automatically. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/update.html for
-- | more info.
update :: forall m. MonadEffect m => ElementWrapper -> m Unit
update = liftEffect <<< runEffectFn1 update_

foreign import configure_ :: EffectFn1 Unit Unit

foreign import mount_ :: EffectFn1 ReactElement ElementWrapper

foreign import shallow_ :: EffectFn1 ReactElement ElementWrapper

foreign import at_ :: EffectFn2 Int ElementWrapper ElementWrapper

foreign import debug_ :: EffectFn1 ElementWrapper String

foreign import exists_ :: EffectFn2 String ElementWrapper Boolean

foreign import find_ :: EffectFn2 String ElementWrapper ElementWrapper

foreign import parent_ :: EffectFn1 ElementWrapper ElementWrapper

foreign import is_ :: EffectFn2 String ElementWrapper Boolean

foreign import prop_ :: forall a. EffectFn2 String ElementWrapper a

foreign import setState_ :: forall state. Fn2 state ElementWrapper (EffectFnAff Unit)

foreign import simulate_ :: EffectFn3 String Foreign ElementWrapper Unit

foreign import simulateCustom_ :: EffectFn3 String Foreign ElementWrapper Unit

foreign import state_ :: EffectFn1 ElementWrapper Foreign

foreign import text_ :: EffectFn1 ElementWrapper String

foreign import name_ :: EffectFn1 ElementWrapper String

foreign import unmount_ :: EffectFn1 ElementWrapper Unit

foreign import update_ :: EffectFn1 ElementWrapper Unit

foreign import data ElementWrapper :: Type
