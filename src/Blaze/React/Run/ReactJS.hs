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

import           GHCJS.Types           (JSVal, JSString,)
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Foreign.QQ      (js, js_)
import           GHCJS.Foreign.Callback   (Callback)
import qualified GHCJS.Foreign.Callback as Callback
import qualified GHCJS.Marshal         as Marshal
import           JavaScript.Object.Internal (Object(Object))
import qualified JavaScript.Object as Object
import qualified Data.JSString.Text as JSString

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

foreign import javascript unsafe
    "h$reactjs.queryLifeCycleNode($1, $2)"
    queryLifeCycleNode
        :: JSVal
        -> Int
        -> IO JSVal

foreign import javascript unsafe
    "h$reactjs.mountApp($1, $2, $3, $4)"
    mountReactApp
        :: JSVal -- ^ Browser DOM node
        -> Callback (JSVal -> IO ())
           -- ^ render callback that stores the created nodes in the 'node'
           -- property of the given object.
        -> Callback (JSVal -> IO ())
        -> Callback (JSVal -> IO ())
        -> IO JSVal

foreign import javascript unsafe
    "h$reactjs.syncRedrawApp($1)"
    syncRedrawApp :: JSVal -> IO ()

foreign import javascript unsafe
    "h$reactjs.attachRouteWatcher($1)"
    attachPathWatcher
        :: Callback (JSVal -> IO ())
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
    requestAnimationFrame :: Callback (IO ()) -> IO ()

atAnimationFrame :: IO () -> IO ()
atAnimationFrame io = do
    cb <- fixIO $ \cb ->
        Callback.syncCallback Callback.ThrowWouldBlock
          (Callback.releaseCallback cb >> io)
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
    urlFragmentStr <- JSString.textFromJSString <$> [js|location.hash|]
    urlFragmentVar <- newIORef urlFragmentStr

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
            setRoute $ JSString.textToJSString $ "#" <> newPath

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

        mkRenderCb :: IO (Callback (JSVal -> IO ()))
        mkRenderCb = do
            Callback.syncCallback1 Callback.ThrowWouldBlock $ \objRef -> do
                state <- readIORef stateVar
                let (WindowState body path) = renderAppState state
                updatePath path
                (node, lifeCycleEventHandlers) <- ReactJS.renderHtml handleAction body
                Object.setProp ("node" :: JSString) node $ Object objRef
                writeIORef lifeCycleEventHandlersVar lifeCycleEventHandlers

    onPathChange <- Callback.syncCallback1 Callback.ThrowWouldBlock $
      \pathStrRef -> do
        currentPath <- readIORef urlFragmentVar
        pathStr <- Marshal.fromJSValUnchecked pathStrRef
        let newPath = T.drop 1 $ JSString.textFromJSString pathStr
        -- FIXME (asayers): if the route is the same, it seems to trigger a
        -- full-page reload
        unless (newPath == currentPath) $ do
          writeIORef urlFragmentVar newPath
          handleAction (PathChangedTo newPath) True
    attachPathWatcher onPathChange

    let mkDidUpdateCb :: IO (Callback (JSVal -> IO ()))
        mkDidUpdateCb =
            Callback.syncCallback1 Callback.ThrowWouldBlock $ \objRef -> do
                lifeCycleEventHandlers <- readIORef lifeCycleEventHandlersVar
                forM_ lifeCycleEventHandlers $ \(elemId, eh) ->
                  case eh of
                    OnDomDidUpdate f -> do
                        node <- queryLifeCycleNode objRef elemId
                        mbDomNode <- Marshal.fromJSVal node
                        case mbDomNode of
                          Just domNode -> do
                            act <- f domNode
                            handleAction act False
                          Nothing -> error "mkDidUpdateDb: couldn't find dom node"

        mkShouldUpdateCb :: IO (Callback (JSVal -> IO ()))
        mkShouldUpdateCb =
            Callback.syncCallback1 Callback.ThrowWouldBlock $ \objRef -> do
              result <- readIORef shouldUpdateVar
              Object.setProp ("result" :: JSString) (Foreign.toJSBool result) $ Object objRef

    -- mount and redraw app
    bracket mkRenderCb Callback.releaseCallback $ \renderCb ->
      bracket mkDidUpdateCb Callback.releaseCallback $ \didUpdateCb ->
        bracket mkShouldUpdateCb Callback.releaseCallback $ \shouldUpdateCb -> do
          app <- mountReactApp root renderCb didUpdateCb shouldUpdateCb
          -- manually tie the knot between the event handlers
          writeIORef rerenderVar (Just (syncRedrawApp app))
          -- start the first drawing
          syncRedraw
          -- handle the initial requests
          handleRequests initialRequests
          -- keep main thread running forever
          forever $ threadDelay 10000000
