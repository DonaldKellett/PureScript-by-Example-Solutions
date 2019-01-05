module Main where

import Prelude
import Effect (Effect, forE)
import Effect.Console (log)
import Effect.Exception
import Effect.Ref
import Data.Int
import Effect.Random

-- Exercise 1
safeDivide :: Int -> Int -> Effect Int
safeDivide _ 0 =
  throwException (error "Dividing an integer by zero is not allowed!")
safeDivide m n = pure (div m n)

-- Exercise 2
piApproxWithNPoints :: Int -> Effect Number
piApproxWithNPoints n = do
  count <- new 0
  forE 0 n $ \_ -> do
    x <- random
    y <- random
    modify_ (\n -> n + (if x * x + y * y <= 1.0
      then 1
      else 0)) count
  n' <- read count
  pure (toNumber (4 * n') / toNumber n)

-- Driver program for testing the exercises above by executing them and
-- printing their results, handling potential exceptions raised during
-- execution
main :: Effect Unit
main = do
  log "Exercise 1: Safe Division using Exceptions"
  catchException
    (\e -> do
      log ("Exception caught in Exercise 1: " <> message e)
      log "Execution of Exercise 1 examples halted.") $ do
    n0 <- safeDivide 9 3
    log ("9 divided by 3 is " <> show n0)
    n1 <- safeDivide 15 2
    log ("15 divided by 2 (using integer division) is " <> show n1)
    n2 <- safeDivide 10 0
    log ("10 divided by 0 is " <> show n2)
    n3 <- safeDivide 101 1
    log ("101 divided by 1 is " <> show n3)
  log "Exercise 2: Approximating Pi using Monte Carlo method"
  pi0 <- piApproxWithNPoints 10
  log ("An approximation of pi using 10 points is: " <> show pi0)
  pi1 <- piApproxWithNPoints 100
  log ("An approximation of pi using 100 points is: " <> show pi1)
  pi2 <- piApproxWithNPoints 1000
  log ("An approximation of pi using 1000 points is: " <> show pi2)
  pi3 <- piApproxWithNPoints 1000000
  log ("An approximation of pi using 10^6 points is: " <> show pi3)
