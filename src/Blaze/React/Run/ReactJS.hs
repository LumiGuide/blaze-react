{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blaze.React.Run.ReactJS
    ( runApp
    , runApp'
    ) where


import           Blaze.React

import           Control.Applicative
import           Control.Concurrent        (threadDelay, forkIO)
import           Control.Exception         (bracket)
import           Control.Monad

import           Data.IORef
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T

import           GHCJS.Types           (JSRef, JSString, JSObject, JSFun, castRef)
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Foreign.QQ      (js, js_)
import qualified GHCJS.Marshal         as Marshal

import           Prelude hiding (div)

import           System.IO             (fixIO)

import qualified Text.Blaze.Renderer.ReactJS    as ReactJS
import           Text.Blaze.Event.Internal (LifeCycleEventHandler(..))


------------------------------------------------------------------------------
-- Generic 'runApp' function based on reactjs
------------------------------------------------------------------------------


-- ISSUES:
--   * 'this' in callbacks
--   * how to return a value from a sync callback


-- | A type-tag for an actual Browser DOM node.
data DOMNode_
data ReactJSApp_

foreign import javascript unsafe
    "h$reactjs.queryLifeCycleNode($1, $2)"
    queryLifeCycleNode
        :: JSRef DOMNode_
        -> Int
        -> IO (JSRef DOMNode_)

foreign import javascript unsafe
    "h$reactjs.mountApp($1, $2, $3, $4)"
    mountReactApp
        :: JSRef DOMNode_                          -- ^ Browser DOM node
        -> JSFun (JSObject ReactJS.ReactJSNode -> IO ())
           -- ^ render callback that stores the created nodes in the 'node'
           -- property of the given object.
        -> JSFun (JSRef DOMNode_ -> IO ())
        -> JSFun (JSObject ReactJS.ReactJSNode -> IO ())
        -> IO (JSRef ReactJSApp_)

foreign import javascript unsafe
    "h$reactjs.syncRedrawApp($1)"
    syncRedrawApp :: JSRef ReactJSApp_ -> IO ()

foreign import javascript unsafe
    "h$reactjs.attachRouteWatcher($1)"
    attachPathWatcher
        :: JSFun (JSString -> IO ())
           -- ^ Callback that handles a route change.
        -> IO ()

foreign import javascript unsafe
    "h$reactjs.setRoute($1)"
    setRoute
        :: JSString
           -- ^ The new URL fragment
        -> IO ()

foreign import javascript unsafe
    "window.requestAnimationFrame($1)"
    requestAnimationFrame :: JSFun (IO ()) -> IO ()

atAnimationFrame :: IO () -> IO ()
atAnimationFrame io = do
    cb <- fixIO $ \cb ->
        Foreign.syncCallback Foreign.AlwaysRetain
                             False
                             (Foreign.release cb >> io)
    requestAnimationFrame cb

runApp' :: (Show act) => App st act -> IO ()
runApp' = runApp . ignoreWindowActions

runApp :: forall st act. (Show act) => App st (WithWindowActions act) -> IO ()
runApp (App initialState initialRequests apply renderAppState) = do
    -- create root element in body for the app
    root <- [js| document.createElement('div') |]
    [js_| document.body.appendChild(`root); |]

    -- state variables
    stateVar           <- newIORef initialState  -- The state of the app
    redrawScheduledVar <- newIORef False         -- True if a redraw was scheduled
    rerenderVar        <- newIORef Nothing       -- IO function to actually render the DOM
    shouldUpdateVar    <- newIORef True

    lifeCycleEventHandlersVar <- newIORef ([] :: [(Int, LifeCycleEventHandler (WithWindowActions act))])

    -- This is a cache of the URL fragment (hash) to prevent unnecessary
    -- updates.
    urlFragmentVar <- newIORef =<< Foreign.fromJSString <$> [js|location.hash|]

    -- rerendering
    let syncRedraw = join $ fromMaybe (return ()) <$> readIORef rerenderVar

        asyncRedraw = do
            -- FIXME (meiersi): there might be race conditions
            redrawScheduled <- readIORef redrawScheduledVar
            unless redrawScheduled $ do
                writeIORef redrawScheduledVar True
                atAnimationFrame $ do
                    writeIORef redrawScheduledVar False
                    syncRedraw

    let updatePath newPath = do
          currentPath <- readIORef urlFragmentVar
          unless (newPath == currentPath) $ do
            writeIORef urlFragmentVar newPath
            setRoute $ Foreign.toJSString $ "#" <> newPath

    -- create render callback for initialState
    let handleAction :: WithWindowActions act -> Bool -> IO ()
        handleAction action requireSyncRedraw = do
            -- print action
            (requests, shouldUpdate) <- atomicModifyIORef' stateVar $ apply action
            writeIORef shouldUpdateVar shouldUpdate
            handleRequests requests
            if requireSyncRedraw then syncRedraw else asyncRedraw

        handleRequests requests = do
          forM_ requests $ \req -> forkIO $ do
            action <- req
            handleAction action False

        mkRenderCb :: IO (JSFun (JSObject ReactJS.ReactJSNode -> IO ()))
        mkRenderCb = do
            Foreign.syncCallback1 Foreign.AlwaysRetain False $ \objRef -> do
                state <- readIORef stateVar
                let (WindowState body path) = renderAppState state
                updatePath path
                (node, lifeCycleEventHandlers) <- ReactJS.renderHtml handleAction body
                Foreign.setProp ("node" :: JSString) node objRef
                writeIORef lifeCycleEventHandlersVar lifeCycleEventHandlers

    onPathChange <- Foreign.syncCallback1 Foreign.AlwaysRetain False $
      \pathStr -> do
        currentPath <- readIORef urlFragmentVar
        let newPath = T.drop 1 $ Foreign.fromJSString pathStr
        -- FIXME (asayers): if the route is the same, it seems to trigger a
        -- full-page reload
        unless (newPath == currentPath) $ do
          writeIORef urlFragmentVar newPath
          handleAction (PathChangedTo newPath) True
    attachPathWatcher onPathChange

    let mkDidUpdateCb :: IO (JSFun (JSRef DOMNode_ -> IO ()))
        mkDidUpdateCb =
            Foreign.syncCallback1 Foreign.AlwaysRetain False $ \objRef -> do
                lifeCycleEventHandlers <- readIORef lifeCycleEventHandlersVar
                forM_ lifeCycleEventHandlers $ \(elemId, eh) ->
                  case eh of
                    OnDomDidUpdate f -> do
                        node <- queryLifeCycleNode objRef elemId
                        mbDomNode <- Marshal.fromJSRef $ castRef node
                        case mbDomNode of
                          Just domNode -> do
                            act <- f domNode
                            handleAction act False
                          Nothing -> error "mkDidUpdateDb: couldn't find dom node"

        mkShouldUpdateCb :: IO (JSFun (JSObject ReactJS.ReactJSNode -> IO ()))
        mkShouldUpdateCb =
            Foreign.syncCallback1 Foreign.AlwaysRetain False $ \objRef -> do
              result <- readIORef shouldUpdateVar
              Foreign.setProp ("result" :: JSString) (Foreign.toJSBool result) objRef

    -- mount and redraw app
    bracket mkRenderCb Foreign.release $ \renderCb ->
      bracket mkDidUpdateCb Foreign.release $ \didUpdateCb ->
        bracket mkShouldUpdateCb Foreign.release $ \shouldUpdateCb -> do
          app <- mountReactApp root renderCb didUpdateCb shouldUpdateCb
          -- manually tie the knot between the event handlers
          writeIORef rerenderVar (Just (syncRedrawApp app))
          -- start the first drawing
          syncRedraw
          -- handle the initial requests
          handleRequests initialRequests
          -- keep main thread running forever
          forever $ threadDelay 10000000
