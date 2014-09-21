import Haste

import Data.Todo

testTodo :: Todo
testTodo = Todo { task="I was created dynamically!"
                , completed=False }

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

handleTodoList :: Elem -> IO ()
handleTodoList ul = do
  todoEl <- newTodoEl testTodo
  addChild todoEl ul
  -- writeLog $ show $ addTodo Todo {task="Say Hello", completed=False} []

main :: IO ()
main = withElem "todo-list" handleTodoList
