module Main where 


import Control.Monad


data Perhaps a 
  = Only a 
  | None
  deriving (Show, Eq)


instance Functor Perhaps where
  fmap _ None = None
  fmap f (Only x) = Only $ f x


instance Applicative Perhaps where
  pure = Only   
  None <*> _ = None
  (Only f) <*> x = fmap f x 


instance Monad Perhaps where
  None >>= _ = None
  (Only x) >>= k = k x


instance MonadFail Perhaps where
  fail _ = None


foo :: Int -> Perhaps (Maybe Int)
foo 0 = Only $ Nothing 
foo x = Only $ Just x


bar :: Int -> Perhaps (Maybe String)
bar x = Only . Just . show $ x


baz :: Int -> Perhaps (Maybe String)
baz x = do
  (Just x) <- foo x
  bar x


main :: IO ()
main = do
  putStrLn $ "baz 0: " ++ show (baz 0)
  putStrLn $ "baz 1: " ++ show (baz 1)
