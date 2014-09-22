module Data.Todo (
  Todo(..),
  TodoList,
  activeTodos,
  completedTodos,
  addTodo,
  mkTaskUpdater,
  toggleCompleted,
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

mkTaskUpdater :: String -> (Todo -> Todo)
mkTaskUpdater s todo = todo {task = s}

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
