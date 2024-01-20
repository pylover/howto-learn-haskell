module Main where


import Data.Array
import Data.Foldable


type Foo = Array Int [Char]


make :: Foo
make = array (0, 2) 
  [ (0, "foo")
  , (1, "bar")
  , (2, "baz")
  ]


printArr :: Foo -> IO ()
printArr x = do
  mapM putStrLn x 
  return ()


main :: IO ()
main = printArr make
