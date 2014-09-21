module Data.Todo (
  Todo(..),
  TodoList,
  addTodo,
  updateTodo,
  removeTodo
  ) where

data Todo = Todo { task :: String
                 , completed :: Bool }
          deriving (Eq, Show)

type TodoList = [Todo]

addTodo :: Todo -> TodoList -> TodoList
addTodo t = (++ [t])

removeTodo ::Todo -> TodoList -> TodoList
removeTodo t  = filter (/= t)

updateTodo :: Todo -> (Todo -> Todo) -> TodoList -> TodoList
updateTodo t f tl = map update tl where
  update ti
    | ti == t    = f t
    | otherwise  = t
