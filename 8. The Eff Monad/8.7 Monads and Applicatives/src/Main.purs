module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array hiding ((:))
import Data.Maybe
import Data.List.Types

-- Exercise 1
third :: forall a. Array a -> Maybe a
third arr = do
  t <- tail arr
  t' <- tail t
  head t'

-- Exercise 2
sums :: Array Int -> Array Int
sums arr = sort $ nubEq $ foldM (\m n -> [m, n, m + n]) 0 arr

-- Exercise 5
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  b <- f x
  let result' = filterM f xs
  if b
    then Cons <$> pure x <*> result'
    else result'

-- Boilerplate code allowing project to be compiled and run
main :: Effect Unit
main = do
  log "Hello sailor!"
