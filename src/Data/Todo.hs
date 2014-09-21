module Data.Todo (
  Todo(..),
  TodoList,
  activeTodos,
  completedTodos,
  addTodo,
  updateTodo,
  removeTodo
  ) where

data Todo = Todo { task :: String
                 , completed :: Bool }
          deriving (Eq, Show)

type TodoList = [Todo]

addTodo :: Todo -> TodoList -> TodoList
addTodo = (:)

removeTodo ::Todo -> TodoList -> TodoList
removeTodo t  = filter (/= t)

updateTodo :: Todo -> (Todo -> Todo) -> TodoList -> TodoList
updateTodo t f tl = map update tl where
  update ti
    | ti == t    = f t
    | otherwise  = t

activeTodos :: TodoList -> TodoList
activeTodos = filter (not . completed)

completedTodos :: TodoList -> TodoList
completedTodos = filter completed
