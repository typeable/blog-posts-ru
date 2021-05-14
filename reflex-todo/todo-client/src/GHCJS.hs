{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module GHCJS where

import Control.Monad
import Data.Functor (($>))
import Data.Text (Text)
import GHCJS.DOM
import GHCJS.DOM.Document
  (createElement, execCommand, getBodyUnchecked)
import GHCJS.DOM.Element as Element hiding (scroll)
import GHCJS.DOM.HTMLElement as HE (focus)
import GHCJS.DOM.HTMLInputElement as HIE (select, setValue)
import GHCJS.DOM.Node (appendChild, removeChild)
import GHCJS.DOM.Types hiding (Event, Text)
import Reflex.Dom as R

toClipboard :: MonadJSM m => Text -> m ()
toClipboard txt = do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  inpEl <- uncheckedCastTo HTMLInputElement <$> createElement doc
    ("textarea" :: Text)
  void $ appendChild body inpEl
  HE.focus inpEl
  HIE.setValue inpEl txt
  HIE.select inpEl
  void $ execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  void $ removeChild body inpEl

copyByEvent :: MonadWidget t m => Text -> Event t () -> m ()
copyByEvent txt ev =
  void $ performEvent $ ev $> toClipboard txt
