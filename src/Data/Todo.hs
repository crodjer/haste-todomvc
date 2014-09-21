module Data.Todo (Todo(..)) where

data Todo = Todo { task :: String
                 , completed :: Bool }
