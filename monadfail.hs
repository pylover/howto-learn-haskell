module Main where 


import Control.Monad


data Foo = Foo {getFoo :: Int} | Bar {getBar :: Int}


foo :: IO Int
foo = return 3


main :: IO ()
main = do
  "3" <- liftM show foo
  putStrLn $ "Hello: " ++ show 3
