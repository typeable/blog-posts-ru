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
import Data.Maybe
import Data.Map as M (fromAscList, elems)
import Data.IntMap as IM
import Data.Monoid
import Data.Text
import GHC.Generics
import Reflex.Dom

import GHCJS

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
  el "title" $ text "TODO App"

type Todos = IntMap Todo

data TodoState
  = TodoDone
  | TodoActive { stateEdit :: Bool }
  deriving (Generic, Eq, Show)

data Todo = Todo
  { todoText  :: Text
  , todoState :: TodoState }
  deriving (Generic, Eq, Show)

newTodo :: Text -> Todo
newTodo todoText = Todo { todoState = TodoActive False, .. }

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
  let
    addNewTodo = \todo -> Endo $ \todos ->
      insert (nextKey todos) (newTodo todo) todos
    newTodoDyn = addNewTodo <$> value iEl
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
    TodoDone         -> todoDone ix todoText
    TodoActive False -> todoActive ix todoText
    TodoActive True  -> todoEditable ix todoText

todoActive
  :: (EventWriter t (Endo Todos) m, MonadWidget t m)
  => Int -> Text -> m ()
todoActive ix todoText = divClass "d-flex border-bottom" $ do
  divClass "p-2 flex-grow-1 my-auto" $
    text todoText
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
  => Int -> Text -> m ()
todoDone ix todoText = divClass "d-flex border-bottom" $ do
  divClass "p-2 flex-grow-1 my-auto" $
    el "del" $ text todoText
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
