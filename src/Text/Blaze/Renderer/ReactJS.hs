{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A preliminary renderer that produces `ReactJS` components when run using
-- GHCJS.
--
module Text.Blaze.Renderer.ReactJS
    ( ReactJSNode
    , renderHtml
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either ( runEitherT, EitherT(..), left)

import qualified Data.ByteString.Char8 as SBC
import qualified Data.HashMap.Strict   as HMS
import           Data.List             (isInfixOf)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.ByteString       as S
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )

import qualified Data.DList            as DL

import qualified GHCJS.Foreign         as Foreign
import qualified GHCJS.Foreign.Callback as Foreign
import           GHCJS.Marshal         as Marshal
import           GHCJS.Types           (JSVal, isUndefined, jsval)
import           Data.JSString         (JSString)
import qualified Data.JSString.Text    as JSString (textToJSString, textFromJSString)
import qualified Data.JSString         as JSString
import qualified JavaScript.Object     as Object
import           JavaScript.Object.Internal (Object(Object))
import           JavaScript.Array      (MutableJSArray)
import qualified JavaScript.Array      as Array

import           Prelude               hiding (span)

import           Text.Blaze.Internal
import           Text.Blaze.Event.Internal
import           Text.Blaze.Event.Charcode   (unCharcode)
import           Text.Blaze.Event.Keycode    (unKeycode)


------------------------------------------------------------------------------
-- FFI to ReactJS
------------------------------------------------------------------------------

type ReactJSEvent = Object
type ReactJSNode  = JSVal
type ReactJSNodes = MutableJSArray


foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, $3)"
    mkReactJSParent
        :: JSString -> Object -> ReactJSNodes -> IO ReactJSNode

foreign import javascript unsafe
    "h$reactjs.mkDomNode($1, $2, [])"
    mkReactJSLeaf :: JSString -> Object -> IO ReactJSNode

foreign import javascript unsafe
    "$1.preventDefault()"
    preventDefault :: ReactJSEvent -> IO ()

foreign import javascript unsafe
    "$1.stopPropagation()"
    stopPropagation :: ReactJSEvent -> IO ()


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------


-- TODO (SM): find a better representation for the rendering of Strings.
-- Probably a DList T.Text with a following concat.

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = getString s
fromChoiceString (String s)     = (s ++)
fromChoiceString (Text s)       = (T.unpack s ++)
fromChoiceString (ByteString s) = (SBC.unpack s ++)
fromChoiceString (PreEscaped x) =
    -- FiXME (SM): here we actually need to unescape!
    case x of
      String s -> (s ++)
      Text   s -> (\k -> T.foldr (:) k s)
      s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (s ++)
    Text   s     -> if "</" `T.isInfixOf` s then id else (\k -> T.foldr (:) k s)
    ByteString s -> if "</" `S.isInfixOf` s then id else (SBC.unpack s ++)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id


-- | Render some 'Markup' to a virtual dom.
--
-- This function is morally pure.
--
render
    :: forall act.
       Show act
    => (act -> Bool -> IO ())  -- ^ Callback for actions raised by event handlers.
    -> Markup act
    -> IO (ReactJSNodes, [(Int, LifeCycleEventHandler act)])
render handleAct0 markup = do
    children <- Array.create
    nodeIdVar <- newIORef 0
    lifeCycleEventHandlers <- go nodeIdVar handleAct0 (\_props -> return ()) children markup
    return (children, DL.toList lifeCycleEventHandlers)
  where
    go :: forall act' b
        . IORef Int
       -> (act' -> Bool -> IO ())
       -> (Object -> IO ())
       -> MutableJSArray
       -> MarkupM act' b
       -> IO (DL.DList (Int, LifeCycleEventHandler act'))
    go nodeIdVar handleAct setProps children html0 = case html0 of
        MapActions f h ->
            (DL.fromList . map (fmap (fmap f)) . DL.toList) <$>
              go nodeIdVar (handleAct . f) setProps children h

        OnEvent handler h -> do
            let setProps' props = do
                    registerEventHandler (handleAct <$> handler) props
                    setProps props

            go nodeIdVar handleAct setProps' children h

        OnLifeCycleEvent handler h -> do
            freshId <- readIORef nodeIdVar
            writeIORef nodeIdVar $! freshId + 1
            freshIdJs <- Marshal.toJSVal freshId
            let setProps' props = do
                    Object.setProp ("data-life-cycle-id" :: JSString) freshIdJs props
                    setProps props
            DL.cons (freshId, handler) <$> go nodeIdVar handleAct setProps' children h

        Parent tag _open _close h -> tagToVNode (staticStringToJs tag) h
        CustomParent tag h        -> tagToVNode (choiceStringToJs tag) h
        Leaf tag _begin _end      -> leafToVNode (staticStringToJs tag)
        CustomLeaf tag _close     -> leafToVNode (choiceStringToJs tag)
        Content content           -> textToVNode (choiceStringToJs content)

        AddAttribute key _preparedKey value h -> do
            setProperty (staticStringToJs key) (jsval $ choiceStringToJs value) h

        AddBoolAttribute key value h -> do
            setProperty (staticStringToJs key) (Foreign.toJSBool value) h

        -- FIXME (SM): This is not going to work in all cases, as 'attributes'
        -- must be set differently from properties.
        AddCustomAttribute key value h ->
            setProperty (choiceStringToJs key) (jsval $ choiceStringToJs value) h

        AddObjectAttribute key object h -> do
            jsObj <- toJSVal_hashMap object
            setProperty (staticStringToJs key) (jsval jsObj) h

        Empty           -> return mempty
        Append h1 h2    -> do
            (<>) <$> go nodeIdVar handleAct setProps children h1
                 <*> go nodeIdVar handleAct setProps children h2
      where
        choiceStringToJs cs = JSString.pack (fromChoiceString cs "")
        staticStringToJs ss = JSString.textToJSString (getText ss)

        setProperty
            :: JSString
            -> JSVal
            -> MarkupM act' b
            -> IO (DL.DList (Int, LifeCycleEventHandler act'))
        setProperty key value content =
            go nodeIdVar handleAct setProps' children content
          where
            setProps' props =
                Object.setProp key value props >> setProps props

        makePropertiesObject = do
            props <- Object.create
            setProps props
            return props

        tagToVNode tag content = do
            props         <- makePropertiesObject
            innerChildren <- Array.create
            es <- go nodeIdVar handleAct (\_props -> return ()) innerChildren content
            node <- mkReactJSParent tag props innerChildren
            Array.push node children
            return es

        leafToVNode tag = do
            props <- makePropertiesObject
            node  <- mkReactJSLeaf tag props
            Array.push node children
            return mempty

        textToVNode :: JSString -> IO (DL.DList (Int, LifeCycleEventHandler act'))
        textToVNode jsText = mempty <$ Array.push (jsval jsText) children

-- TODO (asayers): Something like this should probably be added to GHCJS.Marshall:
-- toJSVal_hashMap :: (IsString a, ToJSVal b)
--                 => HMS.HashMap a b
--                 -> IO (JSVal (HMS.HashMap a b))
toJSVal_hashMap :: HMS.HashMap T.Text T.Text -> IO Object
toJSVal_hashMap hashmap = do
    obj <- Object.create
    let addProp k v = Object.setProp (JSString.textToJSString k)
                                     (jsval $ JSString.textToJSString v) obj
    void $ HMS.traverseWithKey addProp hashmap
    return obj

renderHtml
    :: Show act
    => (act -> Bool -> IO ())
    -> Markup act
    -> IO (ReactJSNode, [(Int, LifeCycleEventHandler act)])
renderHtml handleAction html = do
    (children, lifeCycleEventHandlers) <- render handleAction html
    props <- Object.create
    node <- mkReactJSParent "div" props children
    return (node, lifeCycleEventHandlers)


------------------------------------------------------------------------------
-- Event handler callback construction
------------------------------------------------------------------------------

-- | ReactJS defines the following event types, as of v0.12:
data ReactJSEventType
      -- Clipboard Events
    = OnCopyE | OnCutE | OnPasteE
      -- Keyboard Events
    | OnKeyDownE | OnKeyPressE | OnKeyUpE
      -- Focus Events
    | OnFocusE | OnBlurE
      -- Form Events
    | OnChangeE | OnInputE | OnSubmitE
      -- Mouse Events
    | OnClickE | OnDoubleClickE | OnDragE | OnDragEndE | OnDragEnterE
    | OnDragExitE | OnDragLeaveE | OnDragOverE | OnDragStartE | OnDropE
    | OnMouseDownE | OnMouseEnterE | OnMouseLeaveE | OnMouseMoveE
    | OnMouseOutE | OnMouseOverE | OnMouseUpE
      -- Touch Events
    | OnTouchCancelE | OnTouchEndE | OnTouchMoveE | OnTouchStartE
      -- UI Events
    | OnScrollE
      -- Wheel Events
    | OnWheelE

reactEventName :: ReactJSEventType -> JSString
reactEventName ev = case ev of
    OnCopyE        -> "onCopy"
    OnCutE         -> "onCut"
    OnPasteE       -> "onPaste"
    OnKeyDownE     -> "onKeyDown"
    OnKeyPressE    -> "onKeyPress"
    OnKeyUpE       -> "onKeyUp"
    OnFocusE       -> "onFocus"
    OnBlurE        -> "onBlur"
    OnChangeE      -> "onChange"
    OnInputE       -> "onInput"
    OnSubmitE      -> "onSubmit"
    OnClickE       -> "onClick"
    OnDoubleClickE -> "onDoubleClick"
    OnDragE        -> "onDrag"
    OnDragEndE     -> "onDragEnd"
    OnDragEnterE   -> "onDragEnter"
    OnDragExitE    -> "onDragExit"
    OnDragLeaveE   -> "onDragLeave"
    OnDragOverE    -> "onDragOver"
    OnDragStartE   -> "onDragStart"
    OnDropE        -> "onDrop"
    OnMouseDownE   -> "onMouseDown"
    OnMouseEnterE  -> "onMouseEnter"
    OnMouseLeaveE  -> "onMouseLeave"
    OnMouseMoveE   -> "onMouseMove"
    OnMouseOutE    -> "onMouseOut"
    OnMouseOverE   -> "onMouseOver"
    OnMouseUpE     -> "onMouseUp"
    OnTouchCancelE -> "onTouchCancel"
    OnTouchEndE    -> "onTouchEnd"
    OnTouchMoveE   -> "onTouchMove"
    OnTouchStartE  -> "onTouchStart"
    OnScrollE      -> "onScroll"
    OnWheelE       -> "onWheel"

lookupProp :: JSString -> Object -> EitherT T.Text IO JSVal
lookupProp name obj = do
    prop <- liftIO $ Object.getProp name obj
    if isUndefined prop
      then left err
      else return prop
  where
    err = "failed to get property '" <> JSString.textFromJSString name <> "'."

lookupIntProp :: JSString -> Object -> EitherT T.Text IO Int
lookupIntProp name obj = do
    ref <- lookupProp name obj
    mbInt <- liftIO $ Marshal.fromJSVal ref
    case mbInt of
      Nothing -> left "lookupIntProp: couldn't parse field as Int"
      Just x  -> return x

lookupBoolProp :: JSString -> Object -> EitherT T.Text IO Bool
lookupBoolProp name obj = do
    ref <- lookupProp name obj
    mbBool <- liftIO $ Marshal.fromJSVal ref
    case mbBool of
      Nothing -> left "lookupIntProp: couldn't parse field as Bool"
      Just x  -> return x

lookupDoubleProp :: JSString -> Object -> EitherT T.Text IO Double
lookupDoubleProp name obj = do
    ref <- lookupProp name obj
    mbDouble <- liftIO $ Marshal.fromJSVal ref
    case mbDouble of
      Nothing -> left "lookupDoubleProp: couldn't parse field as Double"
      Just x  -> return x

data Handler
    = IgnoreEvent
    | HandleEvent (IO (Bool -> IO ()))
      -- ^ Contains an IO action which generates the callback to attach to the event

registerEventHandler
    :: EventHandler (Bool -> IO ())
    -> Object
       -- ^ Properties to register the event handler in
    -> IO ()
registerEventHandler eh props = case eh of
    OnKeyDown keys mkAct     -> register True OnKeyDownE      $ \eventRef ->
      handleKeyEvent eventRef keys mkAct
    OnKeyUp keys mkAct       -> register True OnKeyUpE        $ \eventRef ->
      handleKeyEvent eventRef keys mkAct
    OnKeyPress chars mkAct   -> register True OnKeyPressE     $ \eventRef ->
      handleCharEvent eventRef chars mkAct

    OnFocus mkAct            -> register False OnFocusE       $ \_eventRef ->
      return $ Right $ HandleEvent mkAct
    OnBlur mkAct             -> register False OnBlurE        $ \_eventRef ->
      return $ Right $ HandleEvent mkAct

    OnValueChange mkAct      -> register True  OnChangeE      $ \eventRef ->
      runEitherT $ do
        targetRef <- lookupProp "target" eventRef
        valueRef <- lookupProp "value" $ Object targetRef
        txt <- mbToEither "could't convert Text" $ Marshal.fromJSVal valueRef
        return $ HandleEvent $ mkAct txt
    OnCheckedChange mkAct    -> register False OnChangeE      $ \eventRef ->
      runEitherT $ do
        checkedRef <- lookupProp "target" eventRef
        valueRef <- lookupProp "checked" $ Object checkedRef
        checked <- mbToEither "couldn't convert Bool" $ Marshal.fromJSVal valueRef
        return $ HandleEvent $ mkAct checked
    OnSubmit mkAct           -> register True  OnSubmitE      $ \_eventRef ->
      return $ Right $ HandleEvent mkAct

    OnClick btns mkAct       -> register False OnClickE       $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnDoubleClick btns mkAct -> register False OnDoubleClickE $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnMouseDown btns mkAct   -> register False OnMouseDownE   $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnMouseUp btns mkAct     -> register False OnMouseUpE     $ \eventRef ->
      handleMouseEvent eventRef btns mkAct
    OnMouseMove mkAct        -> register False OnMouseMoveE   $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseEnter mkAct       -> register False OnMouseEnterE  $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseLeave mkAct       -> register False OnMouseLeaveE  $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseOver mkAct        -> register False OnMouseOverE   $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef
    OnMouseOut mkAct         -> register False OnMouseOutE    $ \eventRef ->
      runEitherT $ HandleEvent . mkAct <$> getMousePosition eventRef

    OnScroll mkAct           -> register False OnScrollE      $ \eventRef ->
      runEitherT $ do
        targetRef <- lookupProp "target" eventRef
        scrollTop <- lookupIntProp "scrollTop"  $ Object targetRef
        return $ HandleEvent $ mkAct scrollTop

    OnWheel mkAct            -> register False OnWheelE       $ \eventRef ->
      runEitherT $ do
        dx <- lookupDoubleProp "deltaX" eventRef
        dy <- lookupDoubleProp "deltaY" eventRef
        dz <- lookupDoubleProp "deltaZ" eventRef
        let deltaValue = DeltaValue dx dy dz
        deltaMode <- lookupIntProp "deltaMode" eventRef
        domDelta <- case deltaMode of
              0 -> return $ PixelDelta deltaValue
              1 -> return $ LineDelta deltaValue
              2 -> return $ PageDelta deltaValue
              _ -> left "registerEventHandler: unrecognized delta mode"
        return $ HandleEvent $ mkAct domDelta

  where
    mbToEither err m = do
      mbX <- liftIO m
      case mbX of
        Nothing -> left err
        Just x  -> return x

    handleKeyEvent eventRef keys mkAct = runEitherT $ do
        keycode <- lookupIntProp "keyCode" eventRef <|>
                   lookupIntProp "which" eventRef
        if keycode `elem` map unKeycode keys
          then return $ HandleEvent mkAct
          else return $ IgnoreEvent

    handleCharEvent eventRef chars mkAct = runEitherT $ do
        charcode <- lookupIntProp "charCode" eventRef <|>
                    lookupIntProp "which" eventRef
        if charcode `elem` map unCharcode chars
          then return $ HandleEvent mkAct
          else return $ IgnoreEvent


    handleMouseEvent
        :: ReactJSEvent
        -> [MouseButton]
        -> (MousePosition -> IO (Bool -> IO ()))
        -> IO (Either T.Text Handler)
    handleMouseEvent eventRef btns mkAct = runEitherT $ do
        button <- getMouseButton eventRef
        if button `elem` btns
          then HandleEvent . mkAct <$> getMousePosition eventRef
          else return IgnoreEvent

    getMouseButton :: ReactJSEvent -> EitherT T.Text IO MouseButton
    getMouseButton eventRef = do
        button <- lookupIntProp "button" eventRef
        case button of
          0 -> return LeftButton
          1 -> return MiddleButton
          2 -> return RightButton
          _ -> left "getMouseButton: couldn't parse button code"

    getMousePosition :: ReactJSEvent -> EitherT T.Text IO MousePosition
    getMousePosition eventRef = do
        clientX <- lookupIntProp "clientX" eventRef
        clientY <- lookupIntProp "clientY" eventRef
        pageX   <- lookupIntProp "pageX"   eventRef
        pageY   <- lookupIntProp "pageY"   eventRef
        screenX <- lookupIntProp "screenX" eventRef
        screenY <- lookupIntProp "screenY" eventRef

        altKey   <- lookupBoolProp "altKey"   eventRef
        ctrlKey  <- lookupBoolProp "ctrlKey"  eventRef
        metaKey  <- lookupBoolProp "metaKey"  eventRef
        shiftKey <- lookupBoolProp "shiftKey" eventRef

        return MousePosition
          { mpClientX  = clientX
          , mpClientY  = clientY
          , mpPageX    = pageX
          , mpPageY    = pageY
          , mpScreenX  = screenX
          , mpScreenY  = screenY
          , mpAltKey   = altKey
          , mpCtrlKey  = ctrlKey
          , mpMetaKey  = metaKey
          , mpShiftKey = shiftKey
          }

    register
        :: Bool
        -> ReactJSEventType
        -> (ReactJSEvent -> IO (Either T.Text Handler))
           -- ^ Callback to actually handle the event.
        -> IO ()
    register requireSyncRedraw reactEvent extractHandler = do
        -- FIXME (SM): memory leak to to AlwaysRetain. Need to hook-up ReactJS
        -- event handler table with GHCJS GC.
        -- TODO (BvD): Should we use ThrowWouldBlock or ContinueAsync?
        cb <- Foreign.syncCallback1 Foreign.ThrowWouldBlock $ \eventRef -> do
            -- try to extract handler
            let eventObj = Object eventRef
            errOrHandler <- extractHandler eventObj

            case errOrHandler of
              Left err -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventObj
                  stopPropagation eventObj
                  -- print the error
                  let eventName = reactEventName reactEvent

                  eTypeRef <- runEitherT (lookupProp "type" eventObj)
                  eventType <- case eTypeRef of
                    Left _err -> return "Unknown type"
                    Right typeRef -> Marshal.fromJSValUnchecked typeRef

                  putStrLn $ unlines
                    [ "blaze-react - event handling error: " ++ T.unpack err
                    , "Event was " ++ JSString.unpack eventName ++
                      " of type "  ++ JSString.unpack eventType
                    ]
              Right IgnoreEvent -> return ()
              Right (HandleEvent mkHandler) -> do
                  -- prevent default action and cancel propagation
                  preventDefault eventObj
                  stopPropagation eventObj
                  -- run the handler. This triggers a redraw.
                  handler <- mkHandler
                  handler requireSyncRedraw

        Object.setProp (reactEventName reactEvent) (jsval cb) props
