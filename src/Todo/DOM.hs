module Todo.DOM (
  initializeApp
  ) where

import Haste
import Haste.Concurrent
import Data.Todo

-- | Get the template string from HTML source.
todoTemplate :: CIO String
todoTemplate = withElem "template-todo" ((flip getProp) "innerHTML")

mkCheckedProp :: Bool -> String
mkCheckedProp True  = "true"
mkCheckedProp False = ""

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
    setChecked el = setProp el "checked" (mkCheckedProp completed')
    setEditValue el = setProp el "value" task'
    task' = task todo
    completed' = completed todo

-- | Create a new Todo DOM element based on the Todo instance.
newTodoEl :: MVar TodoList -> Todo  -> CIO Elem
newTodoEl tmv todo = do
  wrapperEl <- newElem "div"
  template <- todoTemplate
  setProp wrapperEl "innerHTML" (template)
  li <- withQuerySelectorElem wrapperEl "li" (setElTodo todo)
  _  <- onEvent li OnDblClick (handleDoubleClick li)
  _  <- withQuerySelectorElem li ".edit" (registerUpdate li)
  _  <- withQuerySelectorElem li ".toggle" toggleDone
  return li

  where
    registerUpdate li el = onEvent el OnKeyUp (handleEnterUpdate li el)
                           >> onEvent el OnBlur (doTaskUpdate li el)
    toggleDone el = onEvent el OnClick handleToggleDone

    doTaskUpdate li el = concurrent $ do
      task' <- getProp el "value"
      let updater = updateTodo todo (mkTaskUpdater task')
      updater `fmap` takeMVar tmv >>= storeTodos tmv
      withQuerySelectorElem li "label" (\l -> setProp l "innerHTML" $ task')
      setClass li "editing" False

    handleDoubleClick li _ _ = do
      setClass li "editing" True
      withQuerySelectorElem li ".edit" focus

    handleEnterUpdate li el 13 = doTaskUpdate li el
    handleEnterUpdate _ _ _ = return ()

    handleToggleDone _ _ = concurrent $ do
      updateTodo todo toggleCompleted `fmap` takeMVar tmv >>= storeTodos tmv
      renderApp tmv

-- | Render the actual todo list.
renderTodoList :: MVar TodoList -> TodoList -> Elem -> CIO ()
renderTodoList tmv rTodo ul = do
  _ <- withQuerySelectorElems ul "li" $ mapM $ (flip removeChild) ul
  _ <- mapM addTask rTodo
  return ()

  where
    addTask todo = do
      todoEl <- newTodoEl tmv todo
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
renderApp :: MVar TodoList -> CIO ()
renderApp tmv = do
  todos <- readMVar tmv
  hash <- getHash
  let active = activeTodos todos
      done   = completedTodos todos
  renderFilters hash

  withElems ["toggle-all", "footer"] (mapM_  (setHidden $ todos == []))
  withQuerySelectorElem document "#todo-count" (setActiveCount $ length active)
  withQuerySelectorElem document "#clear-completed" (resetClearCompleted $ length done)
  withElem "todo-list" (renderTodoList tmv $ currentTodos todos hash)
  withElem "toggle-all" $ setToggleAllState (active == [])
  return ()

  where
    setHidden status el = setClass el "hidden" status
    currentTodos todos hash
      | hash == "/active"    = activeTodos todos
      | hash == "/completed" = completedTodos todos
      | otherwise            = todos

    setActiveCount len el = do
      setHidden (len == 0) el
      let iStr = if len == 1 then "item" else "items"
      setProp el "innerHTML" $ "<strong>" ++ (show len) ++ "</strong> " ++ iStr
    resetClearCompleted len el = do
      setHidden (len == 0) el
      setProp el "innerHTML" ("Clear completed (" ++ (show len) ++ ")")
    setToggleAllState st el = setProp el "checked" $ mkCheckedProp  st

-- | Manage the new todo input textbox related events.
manageNewTodo :: MVar TodoList -> Elem -> CIO ()
manageNewTodo tmv el = onEvent el OnKeyUp handleNewTodo >> focus el
  where
    createNewTodo value = concurrent $ do
      (addTodo value False) `fmap` (takeMVar tmv) >>= storeTodos tmv
      setProp el "value" ""
      renderApp tmv
    handleNewTodo k = do
      value <- getProp el "value"
      if k == 13 && length value > 0
        then createNewTodo value
        else return ()

manageToggleAll :: MVar TodoList -> Elem -> CIO ()
manageToggleAll tmv el = onEvent el OnClick handleToggleAll >> return ()
  where
    handleToggleAll _ _  = concurrent $ do
      checked <- (== "true") `fmap` getProp el "checked"
      (map $ setCompleted checked) `fmap` (takeMVar tmv) >>= storeTodos tmv
      renderApp tmv

manageClearCompleted :: MVar TodoList -> Elem -> CIO ()
manageClearCompleted tmv el = onEvent el OnClick handleClear >> return ()
  where
    handleClear _ _ = concurrent $ do
      activeTodos `fmap` takeMVar tmv >>= storeTodos tmv
      renderApp tmv

-- | Setup initial DOM events.
setupEvents :: MVar TodoList -> CIO ()
setupEvents tmv = do
  withElem "new-todo" (manageNewTodo tmv)
  withElem "toggle-all" (manageToggleAll tmv)
  withElem "clear-completed" (manageClearCompleted tmv)
  onHashChange onHashChangeHandler

  where
    onHashChangeHandler _ _ = do
      renderApp tmv

-- | Initialize the Todo App: Setup events and do the first render.
initializeApp :: CIO ()
initializeApp = do
  todosMVar <- newMVar []
  renderApp todosMVar
  setupEvents todosMVar
