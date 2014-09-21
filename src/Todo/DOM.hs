module Todo.DOM (
  initializeApp
  ) where


import Haste
import Data.Todo

todoTemplate :: IO String
todoTemplate = withElem "template-todo" ((flip getProp) "innerHTML")

setElTodo :: Todo -> Elem -> IO Elem
setElTodo todo li = do
  withQuerySelectorElem li "label" setLabel
  withQuerySelectorElem li ".toggle" setChecked
  withQuerySelectorElem li ".edit" setEditValue
  setClass li "completed" completed'
  return li
  where
    setLabel el = setProp el "innerHTML" task'
    setChecked el = case completed' of
      -- TODO: Should be able to use setAttr and removeAttr. API for removeAttr
      -- isn't available.
      True -> setProp el "checked" "true"
      _    -> setProp el "checked" ""
    setEditValue el = setAttr el "value" task'
    task' = task todo
    completed' = completed todo

newTodoEl :: Todo -> IO Elem
newTodoEl todo = do
  wrapperEl <- newElem "div"
  template <- todoTemplate
  setProp wrapperEl "innerHTML" (template)
  el <- withQuerySelectorElem wrapperEl "li" (setElTodo todo)
  return el

renderTodoList :: TodoList -> Elem -> IO ()
renderTodoList todos ul = do
  _ <- withQuerySelectorElems ul "li" $ mapM $ (flip removeChild) ul
  _ <- mapM addTask todos
  return ()

  where
    addTask todo = do
      todoEl <- newTodoEl todo
      addChild todoEl ul

renderFilters :: String -> IO ()
renderFilters hash = do
  _ <- withQuerySelectorElems document "#filters li a" (mapM setHighlight)
  return ()

  where
    setHighlight el = do
      mhref <- getAttr el "href"
      setClass el "selected" $ '#':hash == mhref

renderApp :: String -> TodoList -> IO ()
renderApp hash todos = do
  renderFilters hash
  withQuerySelectorElem document "#todo-count strong" setActiveCount
  withQuerySelectorElem document "#clear-completed" resetClearCompleted
  withElem "todo-list" (renderTodoList currentTodos)
  return ()

  where
    currentTodos
      | hash == "/active"    = active
      | hash == "/completed" = done
      | otherwise            = todos

    setActiveCount el = setProp el "innerHTML" $ show $ length active
    resetClearCompleted el = setProp el "innerHTML" clearCompletedText
    clearCompletedText = "Clear completed (" ++ (show $ length done) ++ ")"

    done = completedTodos todos
    active = activeTodos todos

initializeApp :: TodoList -> IO ()
initializeApp todos = do
  hash <- getHash
  renderApp hash todos
  onHashChange onHashChangeHandler

  where
    onHashChangeHandler _ hash = do
      renderApp hash todos
