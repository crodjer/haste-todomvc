module Todo.DOM (
  initializeApp
  ) where


import Haste
import Haste.Concurrent
import Data.Todo

todoTemplate :: CIO String
todoTemplate = withElem "template-todo" ((flip getProp) "innerHTML")

setElTodo :: Todo -> Elem -> CIO Elem
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

newTodoEl :: Todo -> CIO Elem
newTodoEl todo = do
  wrapperEl <- newElem "div"
  template <- todoTemplate
  setProp wrapperEl "innerHTML" (template)
  el <- withQuerySelectorElem wrapperEl "li" (setElTodo todo)
  return el

renderTodoList :: TodoList -> Elem -> CIO ()
renderTodoList todos ul = do
  _ <- withQuerySelectorElems ul "li" $ mapM $ (flip removeChild) ul
  _ <- mapM addTask todos
  return ()

  where
    addTask todo = do
      todoEl <- newTodoEl todo
      addChild todoEl ul

renderFilters :: String -> CIO ()
renderFilters hash = do
  _ <- withQuerySelectorElems document "#filters li a" (mapM setHighlight)
  return ()

  where
    setHighlight el = do
      mhref <- getAttr el "href"
      setClass el "selected" $ '#':hash == mhref

renderApp :: String -> MVar TodoList -> CIO ()
renderApp hash tmv = do
  todos <- readMVar tmv
  let active = activeTodos todos
  let done = completedTodos todos
  renderFilters hash
  withQuerySelectorElem document "#todo-count strong" (setActiveCount active)
  withQuerySelectorElem document "#clear-completed" (resetClearCompleted done)
  withElem "todo-list" (renderTodoList $ currentTodos todos)
  return ()

  where
    currentTodos todos
      | hash == "/active"    = activeTodos todos
      | hash == "/completed" = completedTodos todos
      | otherwise            = todos

    setActiveCount active el = setProp el "innerHTML" $ show $ length active
    resetClearCompleted done el = setProp el "innerHTML" (clearCompletedText done)
    clearCompletedText done = "Clear completed (" ++ (show $ length done) ++ ")"

initializeApp :: MVar TodoList -> CIO ()
initializeApp todosMVar = do
  hash <- getHash
  renderApp hash todosMVar
  onHashChange onHashChangeHandler

  where
    onHashChangeHandler _ hash = do
      renderApp hash todosMVar
