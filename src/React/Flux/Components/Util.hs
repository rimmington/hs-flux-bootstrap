{-|
Module      : $Header$
Copyright   : (c) 2016 Deakin Software and Technology Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhys.adams@deakin.edu.au>
Stability   : unstable
Portability : portable

Various utilities and re-exports from "React.Flux".
-}

module React.Flux.Components.Util (
      viewWithKey
    -- * JSString
    , JSString, jsPack, jsUnpack, jsToText, elemStr
    -- * Re-exports
    , ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , ReactView, defineView, defineControllerView
    , reactRender
    , elemText
  ) where

import React.Flux.Components.Types (Element, JSString)

import qualified Data.JSString as JSS
import Data.JSString.Text (textToJSString, textFromJSString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import React.Flux (
      ReactView, defineView, defineControllerView
    , ReactStore, StoreData (..), SomeStoreAction (..), alterStore, mkStore
    , reactRender)
import qualified React.Flux as F

-- NOTE: These aren't inverses of each other. This is intentional, if cheeky.
jsUnpack :: Text -> JSString
jsUnpack = textToJSString

jsPack :: String -> JSString
jsPack = JSS.pack

jsToText :: JSString -> Text
jsToText = textFromJSString

-- | A simpler `F.viewWithSKey`.
viewWithKey :: (Typeable a) => ReactView a -> JSString -> a -> Element
viewWithKey v k ps = F.viewWithSKey v k ps mempty

elemStr :: JSString -> Element
elemStr = F.elemJSString

elemText :: Text -> Element
elemText = F.elemText
