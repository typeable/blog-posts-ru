{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor (($>))
import Data.Maybe
import Data.Map as M (fromAscList, elems)
import Data.IntMap as IM
import Data.Monoid
import Data.Text
import Data.Time
import GHC.Generics
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.ScriptDependent

import GHCJS
import JSFFI

main :: IO ()
main = mainWidgetWithHead headWidget rootWidget

headWidget :: MonadWidget t m => m ()
headWidget = do
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta"
    (  "name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no" )
    blank
  elAttr "link"
    (  "rel" =: "stylesheet"
    <> "href" =: "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"
    <> "integrity" =: "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"
    <> "crossorigin" =: "anonymous")
    blank
  elAttr "link"
    (  "rel" =: "stylesheet"
    <> "href" =: "https://cdn.jsdelivr.net/npm/flatpickr/dist/flatpickr.min.css" )
    blank
  elAttr "script"
    (  "src" =: "https://cdn.jsdelivr.net/npm/flatpickr")
    blank
  el "title" $ text "TODO App"

type Todos = IntMap Todo

data TodoState
  = TodoDone
  | TodoActive { stateEdit :: Bool }
  deriving (Generic, Eq, Show)

data Todo = Todo
  { todoText     :: Text
  , todoDeadline :: Day
  , todoState    :: TodoState }
  deriving (Generic, Eq, Show)

newTodo :: Text -> Day -> Todo
newTodo todoText todoDeadline = Todo {todoState = TodoActive False, ..}

startEdit :: Todo -> Todo
startEdit todo = todo { todoState = TodoActive True }

finishEdit :: Text -> Todo -> Todo
finishEdit val todo = todo { todoState = TodoActive False, todoText = val }

toggleTodo :: Todo -> Todo
toggleTodo Todo{..} = Todo {todoState = toggleState todoState,..}
  where
    toggleState = \case
      TodoDone     -> TodoActive False
      TodoActive _ -> TodoDone

nextKey :: IntMap Todo -> Int
nextKey = maybe 0 (succ . fst . fst) . maxViewWithKey

rootWidget :: MonadWidget t m => m ()
rootWidget =
  divClass "container" $ mdo
    elClass "h2" "text-center mt-3" $ text "Todos"
    (_, ev) <- runEventWriterT $ do
      todosDyn <- foldDyn appEndo mempty ev
      newTodoForm
      delimiter
      todoListWidget todosDyn
    blank

newTodoForm :: (EventWriter t (Endo Todos) m, MonadWidget t m) => m ()
newTodoForm = rowWrapper $ el "form" $ divClass "input-group" $ mdo
  iEl <- inputElement $ def
    & initialAttributes .~
      (  "type" =: "text"
      <> "class" =: "form-control"
      <> "placeholder" =: "Todo" )
    & inputElementConfig_setValue .~ ("" <$ btnEv)
  dEl <- inputElement $ def
    & initialAttributes .~
      (  "type" =: "text"
      <> "class" =: "form-control"
      <> "placeholder" =: "Deadline"
      <> "style" =: "max-width: 150px" )
  pb <- getPostBuild
  widgetHoldUntilDefined "flatpickr"
    (pb $> "https://cdn.jsdelivr.net/npm/flatpickr")
    blank
    (addDatePicker dEl)
  today <- utctDay <$> liftIO getCurrentTime
  let
    dateStrDyn = value dEl
    dateDyn = fromMaybe today . parseTimeM True
      defaultTimeLocale "%Y-%m-%d" . unpack <$> dateStrDyn
    addNewTodo = \todo date -> Endo $ \todos ->
      insert (nextKey todos) (newTodo todo date) todos
    newTodoDyn = addNewTodo <$> value iEl <*> dateDyn
    btnAttr = "class" =: "btn btn-outline-secondary"
      <> "type" =: "button"
  (btnEl, _) <- divClass "input-group-append" $
    elAttr' "button" btnAttr $ text "Add new entry"
  let btnEv = domEvent Click btnEl
  tellEvent $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl

todoListWidget
  :: (EventWriter t (Endo Todos) m, MonadWidget t m)
  => Dynamic t Todos -> m ()
todoListWidget todosDyn = rowWrapper $
  void $ listWithKey (M.fromAscList . IM.toAscList <$> todosDyn) todoWidget

rowWrapper :: MonadWidget t m => m a -> m a
rowWrapper ma =
  divClass "row justify-content-md-center" $
    divClass "col-6" ma

delimiter :: MonadWidget t m => m ()
delimiter = rowWrapper $
  divClass "border-top mt-3" blank

todoWidget
  :: (EventWriter t (Endo Todos) m, MonadWidget t m)
  => Int -> Dynamic t Todo -> m ()
todoWidget ix todoDyn' = do
  todoDyn <- holdUniqDyn todoDyn'
  dyn_ $ ffor todoDyn $ \td@Todo{..} -> case todoState of
    TodoDone         -> todoDone ix todoText todoDeadline
    TodoActive False -> todoActive ix todoText todoDeadline
    TodoActive True  -> todoEditable ix todoText

todoActive
  :: (EventWriter t (Endo Todos) m, MonadWidget t m)
  => Int -> Text -> Day -> m ()
todoActive ix todoText deadline = divClass "d-flex border-bottom" $ do
  elClass "p" "p-2 flex-grow-1 my-auto" $ do
    text todoText
    elClass "span" "badge badge-secondary mx-2" $
      text $ pack $ formatTime defaultTimeLocale "%F" deadline
  divClass "p-2 btn-group" $ do
    (copyEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Copy"
    (doneEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Done"
    (editEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Edit"
    (delEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Drop"
    copyByEvent todoText $ domEvent Click copyEl
    tellEvent $ Endo <$> leftmost
      [ update (Just . toggleTodo) ix <$ domEvent Click doneEl
      , update (Just . startEdit) ix  <$ domEvent Click editEl
      , delete ix <$ domEvent Click delEl
      ]

todoDone
  :: (EventWriter t (Endo Todos) m, MonadWidget t m)
  => Int -> Text -> Day -> m ()
todoDone ix todoText deadline = divClass "d-flex border-bottom" $ do
  elClass "p" "p-2 flex-grow-1 my-auto" $ do
    el "del" $ text todoText
    elClass "span" "badge badge-secondary mx-2" $
      text $ pack $ formatTime defaultTimeLocale "%F" deadline
  divClass "p-2 btn-group" $ do
    (doneEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Undo"
    (delEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Drop"
    tellEvent $ Endo <$> leftmost
      [ update (Just . toggleTodo) ix <$ domEvent Click doneEl
      , delete ix <$ domEvent Click delEl
      ]


todoEditable
  :: (EventWriter t (Endo Todos) m, MonadWidget t m)
  => Int -> Text -> m ()
todoEditable ix todoText = divClass "d-flex border-bottom" $ do
  updTodoDyn <- divClass "p-2 flex-grow-1 my-auto" $
    editTodoForm todoText
  divClass "p-2 btn-group" $ do
    (doneEl, _) <- elAttr' "button"
      (  "class" =: "btn btn-outline-secondary"
      <> "type" =: "button" ) $ text "Finish edit"
    let updTodos = \todo -> Endo $ update (Just . finishEdit todo) ix
    tellEvent $
      tagPromptlyDyn (updTodos <$> updTodoDyn) (domEvent Click doneEl)

editTodoForm :: MonadWidget t m => Text -> m (Dynamic t Text)
editTodoForm todo = do
  editIEl <- inputElement $ def
    & initialAttributes .~
      (  "type" =: "text"
      <> "class" =: "form-control"
      <> "placeholder" =: "Todo")
    & inputElementConfig_initialValue .~ todo
  pure $ value editIEl
