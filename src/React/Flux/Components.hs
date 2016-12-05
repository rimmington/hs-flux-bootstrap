{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : $Header$
Copyright   : (c) 2016 Deakin Software and Technology Innovation Lab
License     : BSD3

Maintainer  : Rhys Adams <rhys.adams@deakin.edu.au>
Stability   : unstable
Portability : portable

More-typed DOM elements, properties and Bootstrap components. Doesn't prevent
you from putting an 'li_' in a 'thead_', but does prevent you from putting a
'div_' in a 'checkbox_' or asking for the 'value' of an 'alert_'.
-}

module React.Flux.Components (
    -- * Element types
      Element, Container, Leaf, Leaf'
    , GenContainer, Decorative
    , elemStr, elemText
    -- * Property types
    , Prop, Clickable
    -- * Container elements
    , pageContainer_, pageHeader_
    , section_, div_, p_, span_
    -- * Headings
    , h1_
    -- * Links
    , Link, a_, href, inNewTab, onClick
    -- * Tables
    , table_, thead_, tbody_, tr_, th_, td_
    -- * Lists
    , ul_, li_
    -- * Tabs
    , tabs_, tab_, linkTab_, pager_
    -- * Forms
    , form_, formGroup_, formRow_, formUnlabelledRow_, inputGroup_, inputAddon_
    , HasValue (..)
    , OtherInput
    , TextInput, input_, textarea_, placeholder, select_, option_
    , WordInput, wordInput_
    , Checkbox, checkbox_
    , FileInput, JSFile (..), fileInput_
    , Button, button_, disabled
    -- * ARIA properties
    , AriaRole (..), ariaRole, ariaHidden, ariaSelected, ariaDisabled, ariaLabel, ariaControls
    -- * Misc components
    , Flavour (..), alert_
    , LabelVisibility (..), ProgressLook (..), progressBar_
    , IconType (..), iconMeaning_
    -- * Misc properties
    , htmlId, className, reactKey, title
    -- * Styles
    , Style
    , style, width, displayTable, displayCell, displayFlex, flexGrow, textColour
    , marginTop, marginLow, marginLeft, marginRight, marginX, marginY
    , padTop, padLow, padLeft, padRight, padX, padY
    ) where

import React.Flux.Components.TH (mkContainer, mkRawElem)
import React.Flux.Components.Types
import React.Flux.Components.Util

import Control.DeepSeq (NFData)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHCJS.Marshal (FromJSVal, ToJSVal (toJSVal))
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types (JSVal)
import qualified JavaScript.Object as O
import qualified JavaScript.Object.Internal as O

import React.Flux (($=), (&=))
import qualified React.Flux as F

strProp :: JSString -> JSString -> Prop a
strProp n = Prop . (n $=)
{-# INLINE strProp #-}

jsProp :: (ToJSVal v) => JSString -> v -> Prop a
jsProp n = Prop . (n &=)
{-# INLINE jsProp #-}

htmlId :: JSString -> Prop a
htmlId = strProp "id"

reactKey :: JSString -> Prop a
reactKey = strProp "key"

className :: JSString -> Prop a
className = strProp "className"

data AriaRole = Alert | Button | Link | Presentation | ProgressBar | Search | TabList | Tab
              deriving (Eq, Show)

ariaRoleStr :: AriaRole -> JSString
ariaRoleStr Alert        = "alert"
ariaRoleStr Button       = "button"
ariaRoleStr Link         = "link"
ariaRoleStr Presentation = "presentation"
ariaRoleStr ProgressBar  = "progressbar"
ariaRoleStr Search       = "search"
ariaRoleStr TabList      = "tablist"
ariaRoleStr Tab          = "tab"

ariaRole :: AriaRole -> Prop a
ariaRole = strProp "role" . ariaRoleStr

ariaHidden :: Bool -> Prop a
ariaHidden = jsProp "aria-hidden"

ariaSelected :: Bool -> Prop a
ariaSelected = jsProp "aria-selected"

ariaDisabled :: Bool -> Prop a
ariaDisabled = jsProp "aria-disabled"

ariaLabel :: JSString -> Prop a
ariaLabel = strProp "aria-label"

-- | This element controls another element.
ariaControls :: JSString -- ^ 'htmlId' of the element controlled
             -> Prop a
ariaControls = strProp "aria-controls"

title :: JSString -> Prop a
title = strProp "title"

newtype Style = Style { _unStyle :: [(JSString, JSString)] -> [(JSString, JSString)] }

instance ToJSVal Style where
    toJSVal (Style ssf) = do
        o@(O.Object ov) <- O.create
        mapM_ (\(n, v) -> O.unsafeSetProp n (pToJSVal v) o) $ ssf []
        pure ov

ariaValueNow :: Int -> Prop a
ariaValueNow = jsProp "aria-valuenow"

ariaValueMin :: Int -> Prop a
ariaValueMin = jsProp "aria-valuemin"

ariaValueMax :: Int -> Prop a
ariaValueMax = jsProp "aria-valuemax"

instance Monoid Style where
    mempty = Style id
    (Style a) `mappend` (Style b) = Style $ b . a
    {-# INLINE mappend #-}

style :: Style -> Prop a
style s = Prop $ "style" &= s

txtStyle :: JSString -> JSString -> Style
txtStyle k v = Style ((k, v) :)

-- | Margin for both top and bottom.
marginY :: JSString -> Style
marginY v = marginTop v <> marginLow v

marginTop :: JSString -> Style
marginTop = txtStyle "marginTop"

marginLow :: JSString -> Style
marginLow = txtStyle "marginBottom"

-- | Margin for both left and right.
marginX :: JSString -> Style
marginX v = marginLeft v <> marginRight v

marginLeft :: JSString -> Style
marginLeft = txtStyle "marginLeft"

marginRight :: JSString -> Style
marginRight = txtStyle "marginRight"

-- | Padding for both left and right.
padY :: JSString -> Style
padY v = padTop v <> padLow v

padTop :: JSString -> Style
padTop = txtStyle "paddingTop"

padLow :: JSString -> Style
padLow = txtStyle "paddingBottom"

-- | Padding for both left and right.
padX :: JSString -> Style
padX v = padLeft v <> padRight v

padLeft :: JSString -> Style
padLeft = txtStyle "paddingLeft"

padRight :: JSString -> Style
padRight = txtStyle "paddingRight"

width :: JSString -> Style
width = txtStyle "width"

displayTable :: Style
displayTable = txtStyle "display" "table"

displayCell :: Style
displayCell = txtStyle "display" "table-cell"

displayFlex :: Style
displayFlex = txtStyle "display" "flex"

flexGrow :: Int -> Style
flexGrow = txtStyle "flexGrow" . jsPack . show

textColour :: JSString -> Style
textColour = txtStyle "color"

data Button
-- | A @'Container' 'GenContainer'@ has no special properties.
data GenContainer
data TextInput
data WordInput
data Checkbox
data FileInput
-- | An input-related element without a 'value'.
data OtherInput
data Label
data Link
data Decorative

-- | Something that has a meaningful 'onClick'.
class Clickable a
instance Clickable Button
instance Clickable Link

onClick :: (Clickable a) => (F.Event -> F.MouseEvent -> F.ViewEventHandler) -> Prop a
onClick = Prop . F.onClick

-- | Some form inputs have values.
class HasValue a where
    type Value a :: *

    value :: Value a -> Prop a
    default value :: (ToJSVal (Value a)) => Value a -> Prop a
    value = jsProp "value"

    onChange :: (F.Event -> Value a -> F.ViewEventHandler) -> Prop a
    default onChange :: (FromJSVal (Value a)) => (F.Event -> Value a -> F.ViewEventHandler) -> Prop a
    onChange = Prop . F.onChange . conv
      where
        conv f evt = f evt $ F.target evt "value"

instance HasValue TextInput where
    type Value TextInput = Text

instance HasValue WordInput where
    type Value WordInput = Word
    -- Otherwise display will be as Int
    value = strProp "value" . jsPack . show

instance HasValue Checkbox where
    type Value Checkbox = Bool
    value = jsProp "checked"
    onChange = Prop . F.onChange . conv
      where
        conv f evt = f evt $ F.target evt "checked"

-- | A JavaScript <https://developer.mozilla.org/en/docs/Web/API/File File> object.
newtype JSFile = JSFile JSVal
               deriving (NFData)

instance Show JSFile where
    show _ = "JSFile"

instance HasValue FileInput where
    type Value FileInput = [JSFile]
    -- TODO: yuck
    value = error "cannot set value of a file input"
    onChange = Prop . F.onChange . conv
      where
        conv f evt = f evt $ JSFile <$> F.target evt "files"

$(mkRawElem ''Button "button")
$(mkRawElem ''GenContainer "table")
$(mkRawElem ''TextInput "textarea")
$(mkRawElem ''TextInput "select")

$(concat <$> traverse (mkContainer ''GenContainer)
    ["h1", "ul", "li", "section", "form", "tr", "td", "th", "thead", "tbody", "p", "nav"])
$(mkContainer ''Label "label")
$(mkContainer ''Link "a")
$(mkContainer ''OtherInput "option")

htmlDiv :: RawElem GenContainer
htmlDiv = present "div"

htmlSpan :: RawElem GenContainer
htmlSpan = present "span"

-- | Bootstrap 'h1_' page header.
pageHeader_ :: Container GenContainer
pageHeader_ = containery $ h1 `preset` className "page-header"

-- | Bootstrap's responsive fixed-width page-containing element.
pageContainer_ :: Container GenContainer
pageContainer_ = containery $ htmlDiv `preset` className "container"

div_ :: Container GenContainer
div_ = containery htmlDiv

span_ :: Container GenContainer
span_ = containery htmlSpan

data IconType = IconAlert
              | IconDownload
              deriving (Show, Eq)

iconClass :: IconType -> JSString
iconClass IconAlert    = "glyphicon-alert"
iconClass IconDownload = "glyphicon-download"

-- | An icon that isn't purely decorative.
iconMeaning_ :: IconType
             -> JSString -- ^ Replacement text for screen readers
             -> Leaf Decorative
iconMeaning_ typ msg = leafy $ \(ps :: [Prop Decorative]) ->
    present "span" ps $ span_ [className $ "glyphicon " <> iconClass typ, ariaHidden True] mempty
                     <> span_ [className "sr-only"] (elemStr msg)

-- | Bootstrap's <http://getbootstrap.com/css/#tables-example basically styled table>.
table_ :: Container GenContainer
table_ = containery $ table `preset` className "table"

-- | Bootstrap <http://getbootstrap.com/components/#nav-tabs tab container>. Should contain 'tab_'s.
tabs_ :: Container GenContainer
tabs_ = containery $ \ps -> nav_ . ul_ (className "nav nav-tabs" : ariaRole TabList : ps)

tab_ :: Bool -- ^ Tab is active.
     -> Container GenContainer
tab_ active = containery . act $ li `preset` ariaRole Presentation
  where
    act | active    = (`preset` className "active")
        | otherwise = id

-- | 'tab_' containing a single 'a_' with appropriate ARIA properties.
linkTab_ :: Bool -- ^ Tab is active.
         -> Container Link
linkTab_ active = containery $ \ps -> tab_ active . a_ (ariaRole Tab : ariaSelected active : ps)

-- | Simple <http://getbootstrap.com/components/#pagination-pager Bootstrap pagination buttons>.
pager_ :: JSString -> Container GenContainer
pager_ lbl = containery $ \ps -> nav_ [ariaLabel lbl] . ul_ (className "pager" : ps)

data ProgressLook = Plain | Striped | AnimatedStripes deriving (Show, Eq)

data LabelVisibility = Visible -- ^ Visibile to all visitor
                     | SrOnly  -- ^ Visible to screen readers only
                     deriving (Show, Eq)

-- | Bootstrap <http://getbootstrap.com/components/#progress progress bar>.
progressBar_ :: LabelVisibility
             -> Maybe JSString  -- ^ Message appearing on the bar. Defaults to @n%@.
             -> ProgressLook
             -> Maybe Flavour
             -> Int             -- ^ Progress between 0 and 100
             -> Leaf Decorative
progressBar_ vis lblm lk mflav pct = leafy $ \(ps :: [Prop Decorative]) ->
      present "div" (className "progress" : ps)
    . div_ [className cls, ariaRole ProgressBar, ariaValueMin 0, ariaValueMax 100, ariaValueNow pct, style $ width pctStr]
    . span_ [className spanCls] $ elemStr lbl
  where
    pctStr = jsPack (show pct) <> "%"
    lbl = fromMaybe (pctStr <> suffix) lblm
    spanCls = case vis of
        Visible -> ""
        SrOnly  -> "sr-only"
    cls = "progress-bar" <> stCls <> lkCls
    lkCls = case lk of
        Plain           -> ""
        Striped         -> " progress-bar-striped"
        AnimatedStripes -> " progress-bar-striped active"
    (stCls, suffix) = case mflav of
        Nothing   -> ("", "")
        Just flav -> (" progress-bar-" <> suf, " (" <> suf <> ")")
          where
            suf = flavourSuffix flav

-- | 12-unit wide form row with label.
formRow_ :: JSString -- ^ Unique id
         -> JSString -- ^ Label
         -> Container GenContainer
formRow_ id_ lbl = containery $ \ps -> formGroup_ (reactKey id_ : ps) . (lblElem <>) . div_ [className "col-md-10"]
  where
    lblElem = label_ [for id_, className "col-md-2 control-label"] $ elemStr lbl

-- | Unlabelled form row, 12 units wide with contents aligned with 'formRow_'.
formUnlabelledRow_ :: Container GenContainer
formUnlabelledRow_ = containery $ htmlDiv `preset` className "col-md-offset-2 col-md-10"

href :: JSString -> Prop Link
href = strProp "href"

inNewTab :: Prop Link
inNewTab = strProp "target" "_blank"

for :: JSString -> Prop Label
for = strProp "htmlFor"

-- | @div@ with Bootstrap's @form-group@ utility class.
formGroup_ :: Container GenContainer
formGroup_ = containery $ htmlDiv `preset` className "form-group"

-- | @div@ with Bootstrap's @input-group@ utility class.
inputGroup_ :: Container GenContainer
inputGroup_ = containery $ htmlDiv `preset` className "input-group"

-- | @span@ with Bootstrap's @input-group-addon@ utility class.
inputAddon_ :: Container GenContainer
inputAddon_ = containery $ htmlSpan `preset` className "input-group-addon"

button_ :: Container Button
button_ = containery $ button `preset` className "btn btn-default"

disabled :: Bool -> Prop Button
disabled = jsProp "disabled"

formControlInput :: Prop a -> Leaf a
formControlInput pr = leafy $ flip (present "input" `preset` className "form-control" `preset` pr) mempty

input_ :: Leaf TextInput
input_ = formControlInput (strProp "type" "text" :: Prop TextInput)

wordInput_ :: Leaf WordInput
wordInput_ = formControlInput (strProp "type" "number" :: Prop WordInput)

fileInput_ :: Leaf FileInput
fileInput_ = formControlInput (strProp "type" "file" :: Prop FileInput)

select_ :: Container TextInput
select_ = containery $ select `preset` className "form-control"

checkbox_ :: Leaf Checkbox
checkbox_ = formControlInput (strProp "type" "checkbox" :: Prop Checkbox)

textarea_ :: Leaf TextInput
textarea_ = leafy $ flip (textarea `preset` className "form-control") mempty

placeholder :: JSString -> Prop TextInput
placeholder = strProp "placeholder"

data Flavour = Success | Info | Warning | Danger deriving (Show, Eq)

flavourSuffix :: Flavour -> JSString
flavourSuffix Success = "success"
flavourSuffix Info    = "info"
flavourSuffix Warning = "warning"
flavourSuffix Danger  = "danger"

alert_ :: Flavour -> Container GenContainer
alert_ flav = containery $ htmlDiv `preset` className ("alert alert-" <> flavourSuffix flav) `preset` ariaRole Alert
