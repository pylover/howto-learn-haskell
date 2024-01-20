module Main where


import System.IO


main :: IO ()
-- main = return "Hello" >>= putStrLn
main = putStr ">>> " >> hFlush stdout >> getLine >>= putStrLn >> main
