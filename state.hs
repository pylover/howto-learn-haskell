module Main where


import System.Console.Haskeline


data FooBar = CreateFooBar 
  { getFoo :: Int
  , getBar :: Int
  } 
  deriving Show


continue :: Char -> StateT FooBar IO ()
continue c = lift $ putStrLn [c]


loop :: StateT FooBar InputT IO ()
loop = do
  mayChar <- getInputChar "? "
  case mayChar of 
    Nothing -> do
      outputStrLn "Terminating..."
      return ()
    Just char -> do
      (x, s') <- runStateT s
      lift (continue char) >> loop


main :: IO ()
main = runInputT defaultSettings loop
