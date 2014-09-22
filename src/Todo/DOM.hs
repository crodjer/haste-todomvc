module Todo.DOM (
  initializeApp
  ) where


import Haste
import Haste.Concurrent
import Data.Todo

-- | Get the template string from HTML source.
todoTemplate :: CIO String
todoTemplate = withElem "template-todo" ((flip getProp) "innerHTML")

-- | Update the Todo list to new one.
storeTodos :: MVar TodoList -> TodoList -> CIO ()
storeTodos tmv tl = do
  putMVar tmv tl
  -- Write to localstorage too in future. For now, storeTodos is pretty much
  -- same as putMVar.

-- | Set contents for a Todo Elem based on the Todo instance.
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
    setEditValue el = setProp el "value" task'
    task' = task todo
    completed' = completed todo

-- | Create a new Todo DOM element based on the Todo instance.
newTodoEl :: Todo -> CIO Elem
newTodoEl todo = do
  wrapperEl <- newElem "div"
  template <- todoTemplate
  setProp wrapperEl "innerHTML" (template)
  el <- withQuerySelectorElem wrapperEl "li" (setElTodo todo)
  return el

-- | Render the actual todo list.
renderTodoList :: TodoList -> Elem -> CIO ()
renderTodoList todos ul = do
  _ <- withQuerySelectorElems ul "li" $ mapM $ (flip removeChild) ul
  _ <- mapM addTask todos
  return ()

  where
    addTask todo = do
      todoEl <- newTodoEl todo
      addChild todoEl ul

-- | Render and properly highlight App filters based on hash
renderFilters :: String -> CIO ()
renderFilters hash = do
  _ <- withQuerySelectorElems document "#filters li a" (mapM setHighlight)
  return ()

  where
    setHighlight el = do
      mhref <- getAttr el "href"
      setClass el "selected" $ '#':hash == mhref


-- Render the application: Render todo items based on path and render list
-- overview status.
renderApp :: TodoList -> CIO ()
renderApp todos = do
  hash <- getHash
  renderFilters hash
  withQuerySelectorElem document "#todo-count strong" (setActiveCount)
  withQuerySelectorElem document "#clear-completed" (resetClearCompleted)
  withElem "todo-list" (renderTodoList $ currentTodos hash)
  return ()

  where
    currentTodos hash
      | hash == "/active"    = active
      | hash == "/completed" = done
      | otherwise            = todos

    setActiveCount el = setProp el "innerHTML" $ show $ length active
    resetClearCompleted el = setProp el "innerHTML" (clearCompletedText)
    clearCompletedText = "Clear completed (" ++ (show $ length done) ++ ")"
    active = activeTodos todos
    done = completedTodos todos

renderAppM :: MVar TodoList -> CIO()
renderAppM tmv = readMVar tmv >>= renderApp

-- | Manage the new todo input textbox related events.
manageNewTodo :: MVar TodoList -> Elem -> CIO ()
manageNewTodo tmv el = onEvent el OnKeyUp handleNewTodo >> focus el
  where
    createNewTodo value = concurrent $ do
      let todo = Todo {task=value, completed=False}
      (todo:) `fmap` (takeMVar tmv) >>= storeTodos tmv
      setProp el "value" ""
      renderAppM tmv
    handleNewTodo k = do
      value <- getProp el "value"
      if k == 13 && length value > 0
        then createNewTodo value
        else return ()

-- | Setup initial DOM events.
setupEvents :: MVar TodoList -> CIO ()
setupEvents tmv = do
  withElem "new-todo" (manageNewTodo tmv)
  onHashChange onHashChangeHandler

  where
    onHashChangeHandler _ _ = do
      renderAppM tmv

-- | Initialize the Todo App: Setup events and do the first render.
initializeApp :: MVar TodoList -> CIO ()
initializeApp todosMVar = do
  renderAppM todosMVar
  setupEvents todosMVar
