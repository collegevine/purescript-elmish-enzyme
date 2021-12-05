module Test.Main where

import Prelude

import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Array (reverse)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Elmish.Component (ComponentDef)
import Elmish.Enzyme as Enzyme
import Elmish.Enzyme (find, testComponent, text, (>>))
import Elmish.HTML.Styled as H
import Random.LCG (randomSeed)
import Test.QuickCheck.Gen (Gen, evalGen)
import Test.Spec (SpecT, describe, hoistSpec, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

foreign import _configureJsDomViaFfi :: Type

main :: Effect Unit
main = do
  Enzyme.configure
  runSpec spec

spec :: Spec Unit
spec =
  describe "Foo" $
    it "displays text" $
      testComponent def $
        find ".foo" >> text >>= shouldEqual "Foo"

def :: ComponentDef Void Unit
def =
  { init: pure unit
  , update: \_ _ -> pure unit
  , view: \_ _ ->
      H.div "foo" "Foo"
  }

--
-- Spec Monad — Clean this up
--

type Spec' m i a = SpecT m i Gen a

type Spec a = Spec' (CleanupT Aff) Unit a

runSpec :: Spec Unit -> Effect Unit
runSpec s = do
  let (genSpec :: Gen _) =
        s
        # specWithCleanup
        # runSpecT (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [specReporter]
  seed <- randomSeed
  log $ "Using random seed " <> show seed
  let run = evalGen genSpec { newSeed: seed, size: 10 }
  launchAff_ run

type CleanupT m = WriterT (Array (m Unit)) m

class Monad m <= MonadCleanup m where
  cleanupAction :: m Unit -> CleanupT m Unit

instance MonadEffect m => MonadCleanup m where
  cleanupAction f = tell [f]

specWithCleanup :: forall m g i a. MonadAff g => Monad m => SpecT (CleanupT g) i m a -> SpecT g i m a
specWithCleanup = hoistSpec identity (\_ -> runWithCleanup)

runWithCleanup :: forall m a. MonadAff m => CleanupT m a -> m a
runWithCleanup f = do
  Tuple res cleanupActions <- runWriterT f
  liftAff $ delay $ Milliseconds 0.0  -- allow async stuff to finish running
  for_ (reverse cleanupActions) \a -> a
  pure res
