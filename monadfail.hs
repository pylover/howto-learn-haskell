module Main where 


import Control.Monad


foo :: IO Int
foo = return 3


main :: IO ()
main = do
  "3" <- liftM show foo
  putStrLn $ "Hello: " ++ show 3
