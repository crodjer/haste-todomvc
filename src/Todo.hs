import Haste.Concurrent

import Data.Todo
import Todo.DOM

testTodos :: TodoList
testTodos = [ Todo { identifier=1
                   , task="A task out of fixtures."
                   , completed=False
                   }
            , Todo { identifier=2
                   , task="Another task out of fixtures!"
                   , completed=False
                   }
            , Todo { identifier=3
                   , task="A completed task out of fixtures!"
                   , completed=True
                   }
            , Todo { identifier=4
                   , task="Support dynamically generated tasks."
                   , completed=True
                   }
            ]


main :: IO ()
main = concurrent $ do
  todos <- newMVar testTodos
  initializeApp todos
