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

data Todo = Todo { identifier :: Integer
                 , task :: String
                 , completed :: Bool }
          deriving (Eq, Show)

type TodoList = [Todo]

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
