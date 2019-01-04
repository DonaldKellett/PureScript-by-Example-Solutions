module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Apply
import Data.Maybe

-- Exercise 1
liftAdd :: forall a f. Semiring a => Apply f => f a -> f a -> f a
liftAdd = lift2 (+)

liftSubtract :: forall a f. Ring a => Apply f => f a -> f a -> f a
liftSubtract = lift2 (-)

liftMultiply :: forall a f. Semiring a => Apply f => f a -> f a -> f a
liftMultiply = lift2 (*)

liftDivide :: forall a f. EuclideanRing a => Apply f => f a -> f a -> f a
liftDivide = lift2 (/)

-- Exercise 3
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x

-- Just some template code to allow this project to be compiled and run
main :: Effect Unit
main = do
  log "Hello sailor!"
