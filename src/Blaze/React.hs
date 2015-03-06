{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blaze.React
    ( App(..)
    , WindowState(..), wsPath, wsBody

      -- * Handling window actions
    , WithWindowActions(..)
    , ignoreWindowActions

      -- * Writing transitions
    , Transition
    , TransitionM
    , runTransitionM
    , mkTransitionM
    , zoomTransition

      -- * Controlling virtual DOM updates
    , shouldUpdateDom
    ) where

import Control.Lens (makeLenses, zoom, over, _1, _2, LensLike')
import Control.Lens.Internal.Zoom (Focusing)
import Data.Functor.Identity (Identity)

import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..), getLast)

import qualified Data.Text as T
import           Data.Typeable

import qualified Text.Blaze.Event           as E
import qualified Text.Blaze.Html5           as H

import           Control.Monad.State        (State, runState, get, put)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell, mapWriterT)
import           Control.Monad.Trans.Class  (lift)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data App state action = App
    { appInitialState    :: !state
    , appInitialRequests :: [IO action]
    , appApplyAction     :: !(action -> Transition state action)
    , appRender          :: !(state -> WindowState action)
    }

type Transition  state action = state -> (state, ([IO action], Bool))
type TransitionM state action = WriterT [IO action] (WriterT (Last Bool) (State state)) ()

data WindowState action = WindowState
    { _wsBody  :: !(H.Html action)
    , _wsPath  :: !T.Text
    -- TODO (asayers): _wsTitle :: T.Text
    }

data WithWindowActions act
    = PathChangedTo !T.Text
    | AppAction     !act
    deriving (Show, Typeable, Eq, Ord, Read)

makeLenses ''WindowState

-------------------------------------------------------------------------------
-- Handling window actions
-------------------------------------------------------------------------------

ignoreWindowActions :: App st act -> App st (WithWindowActions act)
ignoreWindowActions (App initialState initialRequests applyAction render) = App
    { appInitialState = initialState
    , appInitialRequests = (map (fmap AppAction) initialRequests)
    , appApplyAction = \action state -> case action of
        PathChangedTo _ -> (state, ([], True))
        AppAction x   -> over (_2 . _1) (map (fmap AppAction)) $ applyAction x state
    , appRender = over wsBody (E.mapActions AppAction) . render
    }

-------------------------------------------------------------------------------
-- Virtual DOM updates
-------------------------------------------------------------------------------

shouldUpdateDom :: Bool -> TransitionM s a
shouldUpdateDom = lift . tell . Last . Just


-------------------------------------------------------------------------------
-- Writing transitions
-------------------------------------------------------------------------------

runTransitionM :: TransitionM s a -> Transition s a
runTransitionM transition oldState =
    let (((_, reqs), shouldUpdate), newState) = runState (runWriterT $ runWriterT transition) oldState
    in (newState, (reqs, fromMaybe True $ getLast shouldUpdate))

mkTransitionM :: Transition s a -> TransitionM s a
mkTransitionM fn = do
    oldState <- get
    let (newState, (writes, shouldUpdate)) = fn oldState
    put newState
    tell writes
    lift $ tell $ Last $ Just shouldUpdate

zoomTransition
    :: forall innerA outerA
              innerS outerS
     . (innerA -> outerA)
    -> LensLike' (Focusing Identity (((), [IO outerA]), Last Bool)) outerS innerS
       -- ^ This can be a @'Lens\'' innerS outerS@ or
       -- @'Traversal\'' innerS outerS@
    -> TransitionM innerS innerA
    -> TransitionM outerS outerA
zoomTransition wrapAction stateLens =
    mapWriterT $
      mapWriterT $
        zoom stateLens
        . fmap (\(((x,                        innerReqs), shouldUpdate)) ->
                  ((x, fmap (fmap wrapAction) innerReqs), shouldUpdate))
