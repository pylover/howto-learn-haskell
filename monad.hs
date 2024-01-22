module Main where


import System.IO


data Calc a 
  = Fail 
  | Ok a
  deriving (Show, Eq) 


instance Functor Calc where
  fmap _ (Fail) = Fail
  fmap f (Ok a) = Ok (f a)


instance Applicative Calc where
  pure = Ok
  Ok f <*> x = fmap f x
  Fail <*> _ = Fail


instance Monad Calc where
  Ok x >>= f = f x
  Fail >>= _ = Fail


add :: Integral a => a ->  a -> Calc a
add x y = Ok $ x + y


half :: Integral a => a -> Calc a
half x
  | even x = return $ x `div` 2
  | otherwise = Fail


aThird :: Integral a => a -> Calc a
aThird x
  | mod x 3 == 0 = Ok $ div x 3
  | otherwise = Fail


foo :: Integral a => a -> Calc a
foo x = do
  x' <- add3 x
  x'' <- aThird x'
  half x''
  where 
    add3 = add 3
  

gather :: Integral a => [Calc a] -> Calc [a]
gather = sequence


main :: IO ()
main = putStrLn . show $ gather (fmap foo l)
  where 
    l = [9, 15, 33]
