module Main where


import Control.Monad.Trans.State
import Control.Arrow


data FooBar a = FooBar 
  { foo :: a
  , bar :: a
  } 
  deriving (Show, Eq)


type FooBarT a m = StateT (FooBar a) m


apply :: (a -> a) -> (a -> a) -> FooBar a -> FooBar a
apply ff fb (FooBar f b) = FooBar (ff f) (fb b)


incrFoo :: Kleisli (FooBarT Int IO) () ()
incrFoo = Kleisli $ \() -> modify $ apply (+1) id 
  

incrBar :: Kleisli (FooBarT Int IO) () ()
incrBar = Kleisli $ \() -> modify $ apply id (+1) 


main :: IO ()
main = execStateT c s >>= putStrLn . show
  where s = FooBar 0 0 
        c = runKleisli a ()
        a = incrFoo >>> incrFoo >>> incrBar
