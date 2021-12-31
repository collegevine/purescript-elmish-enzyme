-- | Provides an FFI bindings for the
-- | [Enzyme](https://enzymejs.github.io/enzyme/) library. This is a pretty thin
-- | wrapper around Enzyme with a few convenience functions added. For a
-- | convenient API, use `Elmish.Enzyme` instead.
module Elmish.Enzyme.Foreign
  ( class Wrapper, toElementWrapper, fromElementWrapper
  , ElementWrapper
  , ChildWrapper
  , RootWrapper
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
  , simulate
  , simulate'
  , simulateCustom'
  , state
  , text
  , toArray
  , unmount
  , unsafeSetState
  , update
  ) where

import Prelude

import Data.Array ((..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFn2, EffectFn3, EffectFnAff, fromEffectFnAff, runEffectFn2, runEffectFn3)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Elmish (ComponentDef, ReactElement, construct)
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Elmish.Enzyme.Adapter (Adapter)
import Foreign (Foreign, unsafeToForeign)
import Unsafe.Coerce (unsafeCoerce)

-- | A generic wrapper returned from, or passed to, an Enzyme function. Some
-- | functions run on any type of wrapper, but some specifically work only at
-- | the top level, or on child nodes.
class Wrapper a where
  toElementWrapper :: a -> ElementWrapper
  fromElementWrapper :: ElementWrapper -> a

-- | Type of generic wrapper used for the FFI.
foreign import data ElementWrapper :: Type

-- | The top-level wrapper, that is, the one that is returned from `mount`.
foreign import data RootWrapper :: Type

instance Wrapper RootWrapper where
  toElementWrapper = unsafeCoerce
  fromElementWrapper = unsafeCoerce

-- | A wrapper that is a child of another wrapper.
foreign import data ChildWrapper :: Type

instance Wrapper ChildWrapper where
  toElementWrapper = unsafeCoerce
  fromElementWrapper = unsafeCoerce

-- | Configures the correct Enzyme adapter. Called once in the main spec.
configure :: Adapter -> Effect Unit
configure = runEffectFn1 configure_

-- | Fully mounts a `ReactElement` and returns a `RootWrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/shallow.html for more info.
mount :: forall m. MonadEffect m => ReactElement -> m RootWrapper
mount =
  liftEffect <<< runEffectFn1 mount_

-- | A convenience function for creating a full wrapper from a `ComponentDef`
-- | rather than a `ReactElement`. This also adds a `delay (Milliseconds 0.0)`
-- | to allow any `Effect`s in `def.init` to run.
mountComponent :: forall m msg state. MonadAff m => ComponentDef msg state -> m RootWrapper
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
shallow :: forall m. MonadEffect m => ReactElement -> m RootWrapper
shallow =
  liftEffect <<< runEffectFn1 shallow_

-- | A convenience function for creating a shallow wrapper from a `ComponentDef`
-- | rather than a `ReactElement`. This also adds a `delay (Milliseconds 0.0)`
-- | to allow any `Effect`s in `def.init` to run.
shallowComponent :: forall m msg state. MonadAff m => ComponentDef msg state -> m RootWrapper
shallowComponent def = do
  wrapper <- liftEffect $ shallow $
    wrapWithLocalState (ComponentName "Shallow Test Component") (const def) {}
  liftAff $ delay (Milliseconds 0.0)
  pure wrapper

-- | A `Wrapper` can have multiple nodes. This gets the node at the given index.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/at.html for
-- | more info.
at :: forall m wrapper. MonadEffect m => Wrapper wrapper => Int -> wrapper -> m ChildWrapper
at idx w = liftEffect $ fromElementWrapper <$> runEffectFn2 at_ idx (toElementWrapper w)

-- | Returns all elements contained in the given `Wrapper` as an array of
-- | `ChildWrapper`s.
toArray :: forall m wrapper. MonadEffect m => Wrapper wrapper => wrapper -> m (Array ChildWrapper)
toArray e = for (0 .. (length e - 1)) \idx -> at idx e

-- | Runs the given action for every element contained in the given `Wrapper`.
forEach :: forall m wrapper. MonadEffect m => Wrapper wrapper => (ChildWrapper -> m Unit) -> wrapper -> m Unit
forEach f e = for_ (0 .. (length e - 1)) \idx -> at idx e >>= f

-- | Prints the shallow DOM tree up to any nested React components.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/debug.html
-- | for more info.
debug :: forall m wrapper. MonadEffect m => Wrapper wrapper => wrapper -> m String
debug = liftEffect <<< runEffectFn1 debug_ <<< toElementWrapper

-- | Returns a `Boolean` indicating whether a given selector exists within the
-- | given `Wrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/exists.html
-- | for more info.
exists :: forall m wrapper. MonadEffect m => Wrapper wrapper => String -> wrapper -> m Boolean
exists selector w = liftEffect $ runEffectFn2 exists_ selector $ toElementWrapper w

-- | Finds all elements with the given selector within the given Wrapper`. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/find.html for
-- | more info.
find :: forall m wrapper. MonadEffect m => Wrapper wrapper => String -> wrapper -> m ChildWrapper
find selector w = liftEffect $ runEffectFn2 find_ selector $ toElementWrapper w

-- | Returns parent of the given `ChildWrapper`. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/parent.html for
-- | more info.
parent :: forall m wrapper. MonadEffect m => Wrapper wrapper => ChildWrapper -> m wrapper
parent w = liftEffect $ fromElementWrapper <$> runEffectFn1 parent_ w

-- | Returns a `Boolean` indicating whether the given `Wrapper` matches the
-- | given selector.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/is.html
-- | for more info.
is :: forall m wrapper. MonadEffect m => Wrapper wrapper => String -> wrapper -> m Boolean
is selector w = liftEffect $ runEffectFn2 is_ selector $ toElementWrapper w

-- | Returns number of elements in a given wrapper
length :: forall wrapper. Wrapper wrapper => wrapper -> Int
length = length_ <<< toElementWrapper

-- | Returns the value of an `Wrapper`’s prop with a certain key.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/prop.html
-- | for more info.
prop :: forall a m wrapper. MonadEffect m => Wrapper wrapper => String -> wrapper -> m a
prop selector w = liftEffect $ runEffectFn2 prop_ selector $ toElementWrapper w

-- | Sets the state of the given `Wrapper`. This is asynchronous, so runs
-- | in a `MonadAff`. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/setState.html for
-- | more info.
-- |
-- | NOTE: this is a type-unsafe operation. There is no check to make sure the
-- | state being set here has the type of the actual state the component in
-- | question is using.
unsafeSetState :: forall state m wrapper. MonadAff m => Wrapper wrapper => state -> wrapper -> m Unit
unsafeSetState newState =
  liftAff <<< fromEffectFnAff <<< runFn2 unsafeSetState_ newState <<< toElementWrapper

-- | A convenience function for calling `simulate'` without an `event` arg.
simulate :: forall m wrapper. MonadEffect m => Wrapper wrapper => String -> wrapper -> m Unit
simulate eventType =
  simulate' eventType {}

-- | Simulates a certain event type on a given `Wrapper`. The event argument is
-- | a record that gets merged with a simulated React synthetic event before
-- | being passed to the component’s event handler. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/simulate.html for
-- | more info.

-- | NOTE: This doesn’t actually emit an event, but just triggers the event
-- | handler on the wrapper.
-- |
-- | NOTE 2: This function only works for native HTML elements. For emitting
-- | events on custom React components, use `simulateCustom`.
simulate' :: forall m r wrapper. MonadEffect m => Wrapper wrapper => String -> Record r -> wrapper -> m Unit
simulate' eventType event =
  liftEffect <<< runEffectFn3 simulate_ eventType (unsafeToForeign event) <<< toElementWrapper

-- | Simulates an event on a custom React component (i.e. not an HTML element).
-- | For reasons to complicated to discuss here, the regular `simulate` doesn't
-- | work on custom components, so we provide this workaround.
-- |
-- | NOTE: the second parameter is passed to the event handler without any
-- | checks whatsoever. This is, of course, not type-safe, but it is in line
-- | with what the event handler should expect anyway: after all, the underlying
-- | JavaScript component may pass anything at all as event argument.
simulateCustom' :: forall m a wrapper. MonadEffect m => Wrapper wrapper => String -> a -> wrapper -> m Unit
simulateCustom' eventType event =
  liftEffect <<< runEffectFn3 simulateCustom_ eventType (unsafeToForeign event) <<< toElementWrapper

-- | Returns the state of the given `Wrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/state.html
-- | for more info.
state :: forall m wrapper. MonadEffect m => Wrapper wrapper => wrapper -> m Foreign
state = liftEffect <<< runEffectFn1 state_ <<< toElementWrapper

-- | Returns the text within the given `Wrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/text.html
-- | for more info.
text :: forall m wrapper. MonadEffect m => Wrapper wrapper => wrapper -> m String
text = liftEffect <<< runEffectFn1 text_ <<< toElementWrapper

-- | Returns tag name of the given `Wrapper`.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/name.html
-- | for more info.
name :: forall m wrapper. MonadEffect m => Wrapper wrapper => wrapper -> m String
name = liftEffect <<< runEffectFn1 name_ <<< toElementWrapper

-- | Returns the number of times a given selector appears within a wrapper.
count :: forall m wrapper. MonadEffect m => Wrapper wrapper => String -> wrapper -> m Int
count selector wrapper = do
  length <$> find selector wrapper

-- | Unmounts a fully mounted component.
-- | See https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/unmount.html
-- | for more info.
unmount :: forall m wrapper. MonadEffect m => Wrapper wrapper => wrapper -> m Unit
unmount =
  liftEffect <<< runEffectFn1 unmount_ <<< toElementWrapper

-- | Updates the given `RootWrapper` to reflect the latest state. Call this
-- | function whenever you think there could be an async change of state that
-- | caused a re-render. For some reason, Enzyme won't pick up the changes
-- | automatically. See
-- | https://enzymejs.github.io/enzyme/docs/api/ReactWrapper/update.html for
-- | more info.
update :: forall m. MonadEffect m => RootWrapper -> m Unit
update = liftEffect <<< runEffectFn1 update_

foreign import configure_ :: EffectFn1 Adapter Unit

foreign import mount_ :: EffectFn1 ReactElement RootWrapper

foreign import shallow_ :: EffectFn1 ReactElement RootWrapper

foreign import at_ :: EffectFn2 Int ElementWrapper ElementWrapper

foreign import debug_ :: EffectFn1 ElementWrapper String

foreign import exists_ :: EffectFn2 String ElementWrapper Boolean

foreign import find_ :: EffectFn2 String ElementWrapper ChildWrapper

foreign import parent_ :: EffectFn1 ChildWrapper ElementWrapper

foreign import length_ :: ElementWrapper -> Int

foreign import is_ :: EffectFn2 String ElementWrapper Boolean

foreign import prop_ :: forall a. EffectFn2 String ElementWrapper a

foreign import unsafeSetState_ :: forall state. Fn2 state ElementWrapper (EffectFnAff Unit)

foreign import simulate_ :: EffectFn3 String Foreign ElementWrapper Unit

foreign import simulateCustom_ :: EffectFn3 String Foreign ElementWrapper Unit

foreign import state_ :: EffectFn1 ElementWrapper Foreign

foreign import text_ :: EffectFn1 ElementWrapper String

foreign import name_ :: EffectFn1 ElementWrapper String

foreign import unmount_ :: EffectFn1 ElementWrapper Unit

foreign import update_ :: EffectFn1 RootWrapper Unit
