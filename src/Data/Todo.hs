module Data.Todo (
  Todo(..),
  TodoList,
  activeTodos,
  completedTodos,
  addTodo,
  mkTaskUpdater,
  setCompleted,
  toggleCompleted,
  updateTodo,
  removeTodo
  ) where

import Haste.Prim
import Haste.JSON
import Haste.Serialize

data Todo = Todo { identifier :: Integer
                 , task :: String
                 , completed :: Bool }
          deriving (Eq, Show, Read)

type TodoList = [Todo]

instance Serialize Todo where
  toJSON = Str . toJSStr . show
  parseJSON (Str s) = (read . fromJSStr) `fmap` return s
  parseJSON _  = error "Tried to parse a non Todo instance as Todo"

addTodo :: String -> Bool -> TodoList -> TodoList
addTodo t c ts = (newTodo:ts)
 where
   nextId = if length ts == 0 then 1
            else (maximum $ map identifier ts) + 1
   newTodo = Todo { identifier = nextId
                  , task = t
                  , completed = c
                  }

removeTodo ::Todo -> TodoList -> TodoList
removeTodo t  = filter (/= t)

mkTaskUpdater :: String -> (Todo -> Todo)
mkTaskUpdater s todo = todo {task = s}

setCompleted :: Bool -> Todo -> Todo
setCompleted state t = t {completed = state}

toggleCompleted :: Todo -> Todo
toggleCompleted todo = todo {completed = not $ completed todo}

updateTodo :: Todo -> (Todo -> Todo) -> TodoList -> TodoList
updateTodo t f tl = map update tl where
  update ti
    | ti == t    = f t
    | otherwise  = ti

activeTodos :: TodoList -> TodoList
activeTodos = filter (not . completed)

completedTodos :: TodoList -> TodoList
completedTodos = filter completed
