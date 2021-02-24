{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.Text
import Reflex.Dom

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

newtype Todo = Todo
  { todoText :: Text }

newTodo :: Text -> Todo
newTodo todoText = Todo {..}

rootWidget :: MonadWidget t m => m ()
rootWidget =
  divClass "container" $ do
    elClass "h2" "text-center mt-3" $ text "Todos"
    newTodoEv <- newTodoForm
    todosDyn <- foldDyn (:) [] newTodoEv
    delimiter
    todoListWidget todosDyn

newTodoForm :: MonadWidget t m => m (Event t Todo)
newTodoForm = rowWrapper $
  el "form" $
    divClass "input-group" $ do
      iEl <- inputElement $ def
        & initialAttributes .~
          (  "type" =: "text"
          <> "class" =: "form-control"
          <> "placeholder" =: "Todo" )
      let
        newTodoDyn = newTodo <$> value iEl
        btnAttr = "class" =: "btn btn-outline-secondary"
          <> "type" =: "button"
      (btnEl, _) <- divClass "input-group-append" $
        elAttr' "button" btnAttr $ text "Add new entry"
      pure $ tagPromptlyDyn newTodoDyn $ domEvent Click btnEl

todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
todoListWidget todosDyn = rowWrapper $
  void $ simpleList todosDyn todoWidget

rowWrapper :: MonadWidget t m => m a -> m a
rowWrapper ma =
  divClass "row justify-content-md-center" $
    divClass "col-6" ma

delimiter :: MonadWidget t m => m ()
delimiter = rowWrapper $
  divClass "border-top mt-3" blank

todoWidget :: MonadWidget t m => Dynamic t Todo -> m ()
todoWidget todoDyn =
  divClass "d-flex border-bottom" $
    divClass "p-2 flex-grow-1 my-auto" $
      dynText $ todoText <$> todoDyn
