module Exercise7 where

import Prelude

import Data.Identity
import Data.Array hiding (foldM)
import Effect
import Effect.Console
import Data.Foldable
import Data.Monoid

-- Exercise 7: Using the Identity monad in the interpretation function
-- (The Identity monad is the trivial monad representing no side-effects :p)

iterM :: forall a m. Monad m => (a -> m a) -> Int -> m a -> m a
iterM _ 0 x = x
iterM f n x = do
  x' <- iterM f (n - 1) x
  f x'

lsystem :: forall a f m s. Monad m => Monad f => Foldable f =>
  f a -> (a -> f a) -> (s -> a -> m s) -> Int -> s -> m s
lsystem init prod interpret n state =
  foldM interpret state $ iterM prod n init

data Alphabet = L | R | F

instance showAlphabet :: Show Alphabet where
  show L = "L"
  show R = "R"
  show F = "F"

type Sentence = Array Alphabet

type State = Sentence

main :: Effect Unit
main = do
  let
    initial :: Sentence
    initial = [F, R, R, F, R, R, F, R, R]
    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions F = [F, L, F, R, R, F, L, F]
    interpret :: State -> Alphabet -> Identity State
    interpret state letter = do
      pure $ snoc state letter
    initialState :: State
    initialState = mempty
  forE 0 6 \i -> do
    log ("Our L-System after " <> show i <> " iterations:")
    log $ show $ lsystem initial productions interpret i initialState
