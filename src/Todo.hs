import Haste

import Data.Todo
import Todo.DOM

testTodos :: TodoList
testTodos = [ Todo { task="A task out of fixtures."
                   , completed=False
                   }
            , Todo { task="Another task out of fixtures!"
                   , completed=False
                   }
            , Todo { task="A completed task out of fixtures!"
                   , completed=True
                   }
            , Todo { task="Support dynamically generated tasks."
                   , completed=True
                   }
            ]


main :: IO ()
main = withElem "todo-list" (renderTodoList testTodos)
