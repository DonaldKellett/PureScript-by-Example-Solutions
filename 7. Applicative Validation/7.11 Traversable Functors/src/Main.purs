module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable
import Data.Traversable

-- Boilerplate for Exercise 1
data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- For debugging purposes
instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch left value right) =
    "Branch (" <> show left <> ") (" <>
      show value <> ") (" <>
      show right <> ")"

instance eqTree :: Eq a => Eq (Tree a) where
  eq Leaf Leaf = true
  eq Leaf _ = false
  eq _ Leaf = false
  eq (Branch left value right) (Branch left' value' right') =
    eq left left' && eq value value' && eq right right'

-- Exercise 1
instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch left value right) =
    Branch (map f left) (f value) (map f right)

instance foldableTree :: Foldable Tree where
  foldr _ seed Leaf = seed
  foldr f seed (Branch left value right) =
    foldr f (f value (foldr f seed right)) left
  foldl _ seed Leaf = seed
  foldl f seed (Branch left value right) =
    foldl f (f (foldl f seed left) value) right
  foldMap _ Leaf = mempty
  foldMap f (Branch left value right) =
    foldMap f left <> f value <> foldMap f right

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch left value right) =
    Branch <$> traverse f left <*> f value <*> traverse f right
  sequence Leaf = pure Leaf
  sequence (Branch left value right) =
    Branch <$> sequence left <*> value <*> sequence right

-- Exercise 3
sequence' :: forall a f t. Traversable t => Applicative f => t (f a) -> f (t a)
-- Source: https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Traversable#t:Traversable
-- Stumbled upon this relation when surfing the docs for the type signatures
-- of traverse and sequence (for Exercise 1).  The type of identity is:
--   forall t a. Category a => a t t
-- which I don't yet understand as of 05/01/2019 but whatever :p
sequence' = traverse identity

traverse' :: forall a b f t. Traversable t => Applicative f => (a -> f b) -> t a -> f (t b)
-- Source: https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Traversable#t:Traversable
-- How it works:
-- 1. The function f has type a -> f b
-- 2. Applying map from the functor instance of t (since Functor is a superclass
--    of Traversable) to our function f gives a new function of type:
--      t a -> t (f b)
-- 3. Since xs has type t a, applying our new function to xs gives
--    a value of type:
--      t (f b)
-- 4. Applying sequence from our functor instance of t with type:
--      t (f b) -> f (t b)
--    onto f <$> xs (of type t (f b)) yields our desired result of type:
--      f (t b)
--    completing our function.
traverse' f xs = sequence (f <$> xs)

-- Just some boilerplate code to allow the project to compile and run
main :: Effect Unit
main = do
  log "Hello sailor!"
