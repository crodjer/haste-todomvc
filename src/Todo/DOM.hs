module Todo.DOM (
  initializeApp
  ) where

import Haste
import Haste.LocalStorage
import Haste.Concurrent
import Data.Todo

-- | Get the template string from HTML source.
todoTemplate :: CIO String
todoTemplate = withElem "template-todo" ((flip getProp) "innerHTML")

mkCheckedProp :: Bool -> String
mkCheckedProp True  = "true"
mkCheckedProp False = ""

-- | Set contents for a Todo Elem based on the Todo instance.
setElTodo :: Todo -> Elem -> CIO Elem
setElTodo todo li = do
  mapQS_ li "label" setLabel
  mapQS_ li ".toggle" setChecked
  mapQS_ li ".edit" setEditValue
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
  li <- mapQS wrapperEl "li" (setElTodo todo) >>= return . (!!0)
  _  <- onEvent li OnDblClick (handleDoubleClick li)
  mapQS_ li ".destroy" destroyEl
  mapQS_ li ".edit" (registerUpdate li)
  mapQS_ li ".toggle" toggleDone
  return li

  where
    registerUpdate li el = onEvent el OnKeyUp (handleEnterUpdate li el)
                           >> onEvent el OnBlur (doTaskUpdate li el)
    toggleDone el = onEvent el OnClick handleToggleDone
    destroyEl el = onEvent el OnClick handleDestroy

    doTaskUpdate li el = concurrent $ do
      task' <- getProp el "value"
      let updater = updateTodo todo (mkTaskUpdater task')
      updater `fmap` takeMVar tmv >>= storeTodos tmv
      mapQS_ li "label" (\l -> setProp l "innerHTML" $ task')
      setClass li "editing" False

    handleDoubleClick li _ _ = do
      setClass li "editing" True
      mapQS_ li ".edit" focus

    handleEnterUpdate li el 13 = doTaskUpdate li el
    handleEnterUpdate _ _ _ = return ()

    handleToggleDone _ _ = concurrent $ do
      updateTodo todo toggleCompleted `fmap` takeMVar tmv >>= storeTodos tmv
      renderApp tmv

    handleDestroy _ _ = concurrent $ do
      removeTodo todo `fmap` takeMVar tmv >>= storeTodos tmv
      renderApp tmv

-- | Render the actual todo list.
renderTodoList :: MVar TodoList -> TodoList -> Elem -> CIO ()
renderTodoList tmv rTodo ul = do
  mapQS_ ul "li" $ (flip removeChild) ul
  _ <- mapM addTask rTodo
  return ()

  where
    addTask todo = do
      todoEl <- newTodoEl tmv todo
      addChild todoEl ul

-- | Render and properly highlight App filters based on hash
renderFilters :: String -> CIO ()
renderFilters hash = do
  mapQS_ document "#filters li a" setHighlight
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

  withElems ["main", "footer"] (mapM_  (setHidden $ todos == []))
  mapQS_ document "#todo-count" (setActiveCount $ length active)
  mapQS_ document "#clear-completed" (resetClearCompleted $ length done)
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

-- | Update the Todo list to new one.
storeTodos :: MVar TodoList -> TodoList -> CIO ()
storeTodos tmv tl = do
  putMVar tmv tl
  writeToStorage tl

writeToStorage :: TodoList -> CIO ()
writeToStorage todos = do
  liftIO $ setItem "todos" todos

loadFromStorage :: CIO TodoList
loadFromStorage = do
  eTodos <- liftIO $ getItem "todos"
  return $ case eTodos of
    Right todos  -> todos
    Left _       -> []

-- | Initialize the Todo App: Setup events and do the first render.
initializeApp :: CIO ()
initializeApp = do
  todos <- loadFromStorage
  todosMVar <- newMVar todos
  renderApp todosMVar
  setupEvents todosMVar
