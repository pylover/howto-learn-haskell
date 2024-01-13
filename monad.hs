module Main where


import Text.Printf (printf)


data State = State 
  { foo :: Int
  , bar :: Int
  }
  deriving (Show, Eq)


newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s) 
  }


instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s


instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')
    m *> k = m >>= \_ -> k


instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'


state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)


get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)


put :: (Monad m) => s -> StateT s m ()
put s = state $ \_ -> ((), s)


modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \s -> ((), f s)


continue :: (Monad m) => Int -> StateT State m ()
continue 5 = return ()
continue x = let inc s = State ((foo s) + 1) ((bar s) * 2)
             in modify inc >> continue (x + 1)


main :: IO ()
main = do
  (_, s) <- runStateT (continue 0) $ (State 0 1)
  putStrLn $ printf "State after: %d %d" (foo s) (bar s) 
