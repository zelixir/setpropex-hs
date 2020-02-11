{-# LANGUAGE DeriveFunctor, TupleSections, ViewPatterns #-}
module State where
import Helper
newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

instance Applicative (State a) where
  pure a = State (a,)
  (<*>) f a = State $ \(runState f -> (f1, s1)) -> first f1 $ runState a s1
  
instance Monad (State a) where
  (>>=) a f = State $ \(runState a -> (a1, s)) -> runState (f a1) s

get = State $ \s -> (s, s)
put s = State $ const ((), s)
state = State
evalState = fst `comp2` runState

