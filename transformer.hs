module Main where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


div' :: Int -> Int -> MaybeT IO Int
div' _ 0 = mzero
div' x y = return $ div x y


mul' :: Int -> Int -> MaybeT IO Int
mul' x y = return $ x * y


main :: IO ()
main = let m = div' 6 2 >>= mul' 4 >>= lift . putStrLn . show
       in runMaybeT m >> return ()
