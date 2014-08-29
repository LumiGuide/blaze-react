{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Blaze.React.Examples.Multiplexer
    ( app
    ) where

import           Prelude hiding (div)

import           Blaze.React        (App(..))
import qualified Blaze.React.Examples.Clock       as Clock
import qualified Blaze.React.Examples.Todo        as Todo
import           Blaze.React.Examples.TimeMachine

import           Control.Applicative
import           Control.Lens       (makeLenses, set, view, Lens')

import qualified Text.Blaze.Html5                 as H
import qualified Text.Blaze.Html5.Attributes      as A


-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

data State = State
    { _dsFocus      :: Focus
    , _dsTodoState  :: TMState Todo.TodoState   Todo.TodoAction
    , _dsClockState :: TMState Clock.ClockState Clock.ClockAction
    }

data Focus
    = TodoF
    | ClockF
    deriving (Eq, Show)

initialState :: State
initialState = State
    { _dsFocus      = TodoF
    , _dsTodoState  = appInitialState (withTimeMachine Todo.app)
    , _dsClockState = appInitialState (withTimeMachine Clock.app)
    }

makeLenses ''State

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

data Action
    = TodoA  (TMAction Todo.TodoAction)
    | ClockA (TMAction Clock.ClockAction)
    | SwitchFocus Focus
    deriving Show

initialDemoRequests :: [IO Action]
initialDemoRequests = concat
    [ fmap TodoA  <$> appInitialRequests (withTimeMachine Todo.app)
    , fmap ClockA <$> appInitialRequests (withTimeMachine Clock.app)
    ]

-------------------------------------------------------------------------------
-- Behaviour
-------------------------------------------------------------------------------

applyAction :: Action -> State -> (State, [IO Action])
applyAction act st = case act of
    TodoA act'  -> foo (appApplyAction (withTimeMachine Todo.app)  act') dsTodoState  TodoA  st
    ClockA act' -> foo (appApplyAction (withTimeMachine Clock.app) act') dsClockState ClockA st
    SwitchFocus focus -> (set dsFocus focus st, [])
  where
    foo :: (a -> (a, [IO b])) -> Lens' s a -> (b -> c) -> s -> (s, [IO c])
    foo fn l c st = let (ist, reqs) = fn (view l st) in (set l ist st, fmap c <$> reqs)

renderState :: State -> H.Html Action
renderState st = do
    H.div H.! A.class_ "multiplexer-app-picker" $ do
      appItem TodoF  "To-do list"
      appItem ClockF "Clock"
    H.div H.! A.class_ "multiplexer-internal-app" $ case view dsFocus st of
      TodoF  -> H.mapActions TodoA  $ appRender (withTimeMachine Todo.app)  $ view dsTodoState  st
      ClockF -> H.mapActions ClockA $ appRender (withTimeMachine Clock.app) $ view dsClockState st
  where
    appItem focus text =
      H.span H.!? (view dsFocus st == focus, A.class_ "multiplexer-active-item")
             H.! H.onClick (SwitchFocus focus) $ text

app :: App State Action
app = App
    { appInitialState    = initialState
    , appInitialRequests = initialDemoRequests
    , appApplyAction     = applyAction
    , appRender          = renderState
    }


