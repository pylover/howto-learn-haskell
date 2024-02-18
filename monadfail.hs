module Main where 


import Control.Monad


foo :: Maybe Int
-- foo = Just 2
foo = Nothing


bar :: Int -> Maybe String
bar x = Just . show $ x


baz :: Maybe String
baz = do
  ~x <- foo
  bar x

main :: IO ()
main = do
  putStrLn $ "Hello: " ++ show baz
