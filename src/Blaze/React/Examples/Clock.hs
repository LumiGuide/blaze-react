{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | A clock app which demonstrates the ability to spawn threads which
-- transform the state when they return.
module Blaze.React.Examples.Clock
    ( app
    , ClockAction(..)
    , ClockState(..)
    ) where

import           Prelude hiding (div)

import           Blaze.React        (App(..))

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Lens       (makeLenses, set)

import           Data.Monoid        ((<>))
import           Data.Time          (UTCTime, getCurrentTime)

import qualified Text.Blaze.Html5   as H


-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

data ClockState = ClockState
    { _csTime :: !(Maybe UTCTime)
    } deriving Show

makeLenses ''ClockState

renderClockState :: ClockState -> H.Html ClockAction
renderClockState (ClockState (Just time)) = "The time is: " <> H.toHtml (show time)
renderClockState (ClockState Nothing)     = "Loading..."

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------

data ClockAction
    = TickA UTCTime
    deriving Show

applyClockAction :: ClockAction -> ClockState -> (ClockState, [IO ClockAction])
applyClockAction action state = case action of
    TickA time -> (set csTime (Just time) state, [scheduleTick])

scheduleTick :: IO ClockAction
scheduleTick = do
  threadDelay 1000000
  TickA <$> getCurrentTime

-------------------------------------------------------------------------------
-- App
-------------------------------------------------------------------------------

app :: App ClockState ClockAction
app = App
    { appInitialState    = ClockState { _csTime = Nothing }
    , appInitialRequests = [scheduleTick]
    , appApplyAction     = applyClockAction
    , appRender          = renderClockState
    }

