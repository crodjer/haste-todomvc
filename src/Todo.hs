import Haste

import Data.Todo

main :: IO ()
main = writeLog $ show $ addTodo (Todo {task="Say Hello", completed=False}) []
