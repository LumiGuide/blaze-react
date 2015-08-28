{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.Event.Internal
    ( EventHandler(..)
    , LifeCycleEventHandler(..)

    , MouseButton(..)
    , MousePosition(..)
    , DomDelta(..)
    , DeltaValue(..)
    , File(..)
    , DomNode(..)
    , DomRect(..)
    ) where

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)

import           Text.Blaze.Event.Keycode  (Keycode)
import           Text.Blaze.Event.Charcode (Charcode)

import           GHCJS.Foreign (getPropMaybe)
import qualified GHCJS.Marshal as Marshal
import           GHCJS.Types (JSString, JSRef)
import           GHCJS.Foreign.QQ (js)


-- | One specific and incomplete specifications of event-handlers geared
-- towards their use with ReactJS.
data EventHandler a
    = OnKeyDown  [Keycode]  (IO a)
    | OnKeyUp    [Keycode]  (IO a)
    | OnKeyPress [Charcode] (IO a)

    | OnFocus (IO a)
    | OnBlur  (IO a)

    -- NOTE (asayers): In ReactJS, I believe OnInput has the same semantics as
    -- OnChange, so I won't bother adding it here.
    -- NOTE (asayers): Using the 'selected' attribute of <option> elements
    -- seems to be discouraged in ReactJS (doing this throws a warning).
    -- Therefore I'm removing OnSelectedChange in favour of using OnValueChange
    -- on the <select> element.
    | OnValueChange    (T.Text -> IO a)
    | OnCheckedChange  (Bool   -> IO a)
    | OnSubmit (IO a)

    | OnClick       [MouseButton] (MousePosition -> IO a)
    | OnDoubleClick [MouseButton] (MousePosition -> IO a)
    | OnMouseDown   [MouseButton] (MousePosition -> IO a)
    | OnMouseUp     [MouseButton] (MousePosition -> IO a)
    | OnMouseMove                 (MousePosition -> IO a)
    | OnMouseEnter                (MousePosition -> IO a)
    | OnMouseLeave                (MousePosition -> IO a)
    | OnMouseOver                 (MousePosition -> IO a)
    | OnMouseOut                  (MousePosition -> IO a)

    | OnScroll (Int -> IO a)
    | OnWheel (DomDelta -> IO a)

    -- TODO (asayers): Implement these
    -- OnCopy  ([File] -> IO a)
    -- OnCut   ([File] -> IO a)
    -- OnPaste ([File] -> IO a)

    -- TODO (asayers): Implement these.
    -- OnDrag      ([File] -> IO a)
    -- OnDragEnd   ([File] -> IO a)
    -- OnDragEnter ([File] -> IO a)
    -- OnDragExit  ([File] -> IO a)
    -- OnDragLeave ([File] -> IO a)
    -- OnDragOver  ([File] -> IO a)
    -- OnDragStart ([File] -> IO a)
    -- OnDrop      ([File] -> IO a)

    -- NOTE (asayers): These events require special initialization in React,
    -- and aren't supported by jQuery, so I'll omit them for now.
    -- OnTouchCancel (IO a)
    -- OnTouchEnd    (IO a)
    -- OnTouchMove   (IO a)
    -- OnTouchStart  (IO a)
    deriving (Functor)

data LifeCycleEventHandler a
   = OnDomDidUpdate (DomNode -> IO a)
     deriving (Functor)

data MouseButton = LeftButton | RightButton | MiddleButton deriving (Eq, Show)
data MousePosition = MousePosition
    { mpClientX :: Int
      -- ^ x-position relative to the upper-left corner of the viewport
    , mpClientY :: Int
      -- ^ y-position relative to the upper-left corner of the viewport
    , mpPageX   :: Int
      -- ^ x-position relative to the upper-left corner of the content-area
    , mpPageY   :: Int
      -- ^ y-position relative to the upper-left corner of the content-area
    , mpScreenX :: Int
      -- ^ x-position relative to the upper-left corner of the physical screen
    , mpScreenY :: Int
      -- ^ y-position relative to the upper-left corner of the physical screen
    , mpAltKey   :: Bool
    , mpCtrlKey  :: Bool
    , mpMetaKey  :: Bool
    , mpShiftKey :: Bool
    } deriving (Eq, Show)

data DomDelta
    = PixelDelta DeltaValue
    | LineDelta  DeltaValue
    | PageDelta  DeltaValue
      deriving (Show)

data DeltaValue = DeltaValue { deltaX :: Double, deltaY :: Double, deltaZ :: Double }
                deriving (Show)

data File = File
    { fileName         :: T.Text
    , fileMimeType     :: T.Text
    , fileSize         :: Int    -- ^ Size of the blob in bytes
    , fileLastModified :: UTCTime
    , fileRead         :: IO BS.ByteString -- ^ Read the contents of the blob
    }

data DomNode =
     DomNode
     { domNodeClassName :: !T.Text
     , domNodeId        :: !T.Text
     , domNodeTagName   :: !T.Text
     , domNodeBoundingClientRect :: !DomRect
     } deriving (Eq, Show)

data DomRect =
     DomRect
     { domRectBottom :: !Int
     , domRectHeight :: !Int
     , domRectLeft   :: !Int
     , domRectRight  :: !Int
     , domRectTop    :: !Int
     , domRectWidth  :: !Int
     } deriving (Eq, Show)

instance Marshal.FromJSRef DomNode where
    fromJSRef nodeRef = runMaybeT $
        DomNode <$> lookupProp' "className"
                <*> lookupProp' "id"
                <*> lookupProp' "tagName"
                <*> lookupBoundingClientRect
      where
        lookupProp' name = lookupProp name nodeRef

        lookupBoundingClientRect :: MaybeT IO DomRect
        lookupBoundingClientRect = MaybeT $ do
           rectRef <- [js| `nodeRef.getBoundingClientRect() |]
           Marshal.fromJSRef rectRef


instance Marshal.FromJSRef DomRect where
    fromJSRef rectRef = runMaybeT $
        DomRect <$> lookupProp' "bottom"
                <*> lookupProp' "height"
                <*> lookupProp' "left"
                <*> lookupProp' "right"
                <*> lookupProp' "top"
                <*> lookupProp' "width"
      where
        lookupProp' name = lookupProp name rectRef

lookupProp :: (Marshal.FromJSRef b) => JSString -> JSRef a -> MaybeT IO b
lookupProp name obj = do
    propRef <- MaybeT $ getPropMaybe name obj
    propVal <- MaybeT $ Marshal.fromJSRef propRef
    return propVal
