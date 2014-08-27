{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}

module Main (main) where

import           Prelude hiding (div)


import           Control.Applicative
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (bracket)
import           Control.Monad

import           Data.IORef
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Time             (getCurrentTime)
import qualified Data.Text             as T

import           GHCJS.Types           (JSRef, JSString, JSFun, JSObject)
import qualified GHCJS.Foreign         as Foreign
import           GHCJS.Foreign.QQ      (js, js_)
import qualified GHCJS.Prim            as Prim

import           Safe                  (readMay)

import           System.IO             (fixIO)

import           TodoApp (App(..), DOMEvent(..), todoApp, TodoEventHandler(..))

import qualified Text.Blaze.Renderer.ReactJS    as ReactJS


------------------------------------------------------------------------------
-- Our main function
------------------------------------------------------------------------------

main :: IO ()
main = runApp todoApp todoEventHandlerTypes


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
    "h$reactjs.mountApp($1, $2)"
    mountReactApp
        :: JSRef DOMNode_                          -- ^ Browser DOM node
        -> JSFun (JSObject ReactJS.ReactJSNode -> IO ())
           -- ^ render callback that stores the created nodes in the 'node'
           -- property of the given object.
        -> IO (JSRef ReactJSApp_)

foreign import javascript unsafe
    "h$reactjs.syncRedrawApp($1)"
    syncRedrawApp :: JSRef ReactJSApp_ -> IO ()

foreign import javascript unsafe
    "$1.preventDefault()"
    preventDefault :: ReactJS.ReactJSEvent -> IO ()

foreign import javascript unsafe
    "$1.stopPropagation()"
    stopPropagation :: ReactJS.ReactJSEvent -> IO ()

foreign import javascript unsafe
  "$1.currentTarget.getAttribute(\"data-blaze-id\")"
  rawLookupBlazeId :: ReactJS.ReactJSEvent -> IO JSString

lookupBlazeId :: ReactJS.ReactJSEvent -> IO (Maybe String)
lookupBlazeId eventRef = do
    mbNameRef <- rawLookupBlazeId eventRef
    return $ if Prim.isNull mbNameRef
               then Nothing
               else Just (Prim.fromJSString mbNameRef)

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

todoEventHandlerTypes :: TodoEventHandler -> [ReactJS.EventType]
todoEventHandlerTypes eh = case eh of
    CreateItemEH     -> []
    ToggleItemEH _   -> [ReactJS.Click]
    DeleteItemEH _   -> [ReactJS.Click]
    EditItemEH _     -> [ReactJS.DoubleClick]
    EditInputEH      -> []
    ToggleAllEH      -> [ReactJS.Click]
    ClearCompletedEH -> [ReactJS.Click]


runApp
    :: (Show eh, Read eh, Show act)
    => App st act eh
    -> (eh -> [ReactJS.EventType])
    -> IO ()
runApp (App initialState apply renderAppState handleEvent) toEventTypes = do
    -- create root element in body for the app
    root <- [js| document.createElement('div') |]
    [js_| document.body.appendChild(`root); |]

    -- state variables
    stateVar           <- newIORef initialState  -- The state of the app
    redrawScheduledVar <- newIORef False         -- True if a redraw was scheduled
    rerenderVar        <- newIORef Nothing       -- IO function to actually render the DOM

    -- rerendering
    let scheduleRedraw = do
            -- FIXME (meiersi): there might be race conditions
            redrawScheduled <- readIORef redrawScheduledVar
            unless redrawScheduled $ do
                writeIORef redrawScheduledVar True
                atAnimationFrame $ do
                    writeIORef redrawScheduledVar False
                    join $ fromMaybe (return ()) <$> readIORef rerenderVar

    -- create event handler callback
    let mkEventHandlerCb :: IO (JSFun (ReactJS.ReactJSEvent -> IO ()))
        mkEventHandlerCb = Foreign.syncCallback1 Foreign.AlwaysRetain False $ \eventRef -> do
            -- prevent default action and cancel propagation
            preventDefault eventRef
            stopPropagation eventRef

            -- extract necessary context
            mbType    <- Foreign.getPropMaybe ("type" :: JSString) eventRef
            mbBlazeId <- lookupBlazeId eventRef
            t         <- getCurrentTime

            let errOrMbAction = do
                    domEvent <- case Foreign.fromJSString <$> mbType of
                      Nothing         -> Left "no event type"
                      Just "click"    -> return OnClick
                      Just "dblclick" -> return OnDoubleClick
                      Just otherType       ->
                        Left $ "unhandled event-type '" <> otherType <> "'."
                    blazeIdStr <- maybe (Left "data-blaze-id attribute missing") return mbBlazeId
                    blazeId    <- maybe (Left "failed to parse blaze-id") return (readMay blazeIdStr)
                    -- TODO (meiersi): use time from event object
                    return ((t, domEvent, blazeId), handleEvent t domEvent blazeId)

            case errOrMbAction of
              Left err ->
                  putStrLn $ "runApp - event handling error: " ++ T.unpack err
              Right (eventInfo, Nothing) ->
                  putStrLn $ "runApp - event rejected: " ++ show eventInfo
              Right (eventInfo, Just action) -> do
                  putStrLn $ "runApp - handling: " ++ show eventInfo ++ " ==> " ++ show action
                  atomicModifyIORef' stateVar (\state -> (apply action state, ()))
                  scheduleRedraw


    -- create render callback for initialState
    let mkRenderCb
            :: JSFun (ReactJS.ReactJSEvent -> IO ())
            -> IO (JSFun (JSObject ReactJS.ReactJSNode -> IO ()))
        mkRenderCb eventHandlerCb = do
            Foreign.syncCallback1 Foreign.AlwaysRetain False $ \objRef -> do
                state <- readIORef stateVar
                node <- ReactJS.renderHtml eventHandlerCb toEventTypes (renderAppState state)
                Foreign.setProp ("node" :: JSString) node objRef



    -- mount and redraw app
    bracket mkEventHandlerCb Foreign.release $ \eventHandlerCb ->
        bracket (mkRenderCb eventHandlerCb) Foreign.release $ \renderCb -> do
            app <- mountReactApp root renderCb
            -- manually tie the knot between the event handlers
            writeIORef rerenderVar (Just (syncRedrawApp app))
            -- start the first drawing
            scheduleRedraw
            -- keep main thread running forever
            forever $ threadDelay 10000000

{-

lookupEventHandlerName :: JSRef () -> IO (Maybe String)
lookupEventHandlerName eventRef = do
    mbNameRef <- js_lookupEventHandlerName eventRef
    return $
        if Prim.isNull mbNameRef
          then Nothing
          else Just (Prim.fromJSString mbNameRef)

foreign import javascript unsafe
  "$1.target.getAttribute(\"data-on-blaze-event\")"
  js_lookupEventHandlerName :: JSRef () -> IO JSString

runApp :: (Show eh, Read eh, Show act) => App st act eh -> IO ()
runApp (App initialState apply renderAppState handleEvent) = do
    -- create root element in body for the app
    root <- [js| document.createElement('div') |]
    [js_| document.body.appendChild(`root); |]

    -- create virtual DOM node corresponding to the empty root div
    rootVNode <- VirtualDom.vnode "div"
                   <$> VirtualDom.newProperties
                   <*> VirtualDom.newChildren

    -- create global state variable
    stateVar <- newMVar (False, initialState, rootVNode)

    -- setup event handlers
    let redraw :: IO ()
        redraw = modifyMVar_ stateVar $ \(_requestedRedraw, state, oldVNode) -> do
            newVNode <- Blaze.VirtualDom.renderHtml (renderAppState state)
            patch <- evaluate (VirtualDom.diff oldVNode newVNode)
            VirtualDom.applyPatch root patch
            return (False, state, newVNode)


        installEventHandler :: JSString -> IO DOMEvent -> IO ()
        installEventHandler jsDomEventName mkDomEvent = do
          cb <- Foreign.syncCallback1 Foreign.AlwaysRetain False $ \event -> do
                  mbEventHandlerName <- lookupEventHandlerName event
                  case mbEventHandlerName of
                    Nothing -> putStrLn "No event handler found."
                    Just eventHandlerName -> case readMay eventHandlerName of
                      Nothing -> putStrLn $
                          "Could not parse event handler name: " ++ eventHandlerName
                      Just eventHandler -> do
                        t        <- getCurrentTime
                        domEvent <- mkDomEvent
                        case handleEvent t domEvent eventHandler of
                          Nothing -> putStrLn $
                            "Event handler '" ++ show eventHandler ++ "' rejected '" ++ show domEvent ++ "."
                          Just action -> do
                            putStrLn $ "Event handler '" ++ show (t, domEvent, eventHandler) ++
                                       "' generated action: " ++ show action
                            modifyMVar_ stateVar $ \(requestedRedraw, state, oldVNode) -> do
                                unless requestedRedraw $ atAnimationFrame redraw
                                return (True, apply action state, oldVNode)

          [js_| `root.addEventListener(`jsDomEventName, `cb, false)|]

    -- install click event handler on the root
    installEventHandler "click"    (return OnClick)
    installEventHandler "dblclick" (return OnDoubleClick)

    -- request a redraw for the initial state
    atAnimationFrame redraw

    putStrLn "Started app"

    -- FIXME (SM): it seems as if we need to keep the main thread running to
    -- ensure that the callbacks still work. This is suboptimal.
    forever $ threadDelay 10000000

-}
