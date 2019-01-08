module Exercise6 where

import Prelude

import Data.Maybe
import Data.Array hiding (foldM)
import Effect
import Graphics.Canvas
import Math as Math
import Effect.Console
import Data.Foldable
import Data.HeytingAlgebra

iterM :: forall a m. Monad m => (a -> m a) -> Int -> m a -> m a
iterM _ 0 x = x
iterM f n x = do
  x' <- iterM f (n - 1) x
  f x'

lsystem :: forall a f m s. Monad m => Monad f => Foldable f =>
  f a -> (a -> f a) -> (s -> a -> m s) -> Int -> s -> m s
lsystem init prod interpret n state =
  foldM interpret state $ iterM prod n init

-- Advanced L-System
-- Notice that in the production rules of F and M,
-- every F and M are mirrored, and likewise every
-- L and R are mirrored.  So to simplify the encoding
-- of the production rules, we can employ the following
-- isomorphism:
-- R false <=> L
-- R true <=> R
-- M false <=> F
-- M true <=> M
-- This is a step-up from the suggested:
--   data Alphabet = L | R | F Boolean
-- in PureScript by Example
data Alphabet = R Boolean | M Boolean

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      let
        initial :: Sentence
        initial = [M true]
        productions :: Alphabet -> Sentence
        productions (M bool) =
          [ M bool
          , R bool
          , M (not bool)
          , R bool
          , M bool
          , R (not bool)
          , M (not bool)
          , R (not bool)
          , M bool
          , R (not bool)
          , M (not bool)
          , R (not bool)
          , M bool
          , R bool
          , M (not bool)
          , R bool
          , M bool
          ]
        productions letter = [letter]
        interpret :: State -> Alphabet -> Effect State
        interpret state (R bool) =
          pure $ state { theta = state.theta + (if bool
            then 1.0
            else -1.0) * Math.pi / 3.0 }
        interpret state (M _) = do
          let x = state.x + Math.cos state.theta * 1.5
              y = state.y + Math.sin state.theta * 1.5
          _ <- lineTo ctx x y
          pure { x, y, theta: state.theta }
        initialState :: State
        initialState = { x: 100.0, y: 100.0, theta: 0.0 }
      _ <- setFillStyle ctx "#AA2233"
      _ <- fillPath ctx do
        moveTo ctx initialState.x initialState.y
        _ <- lsystem initial productions interpret 4 initialState
        closePath ctx
        setShadowOffsetX ctx 25.0
        setShadowOffsetY ctx (-25.0)
        setShadowBlur ctx 25.0
        setShadowColor ctx "#444444"
      pure unit
    Nothing -> error "Failed to retrieve HTML5 canvas with id \"canvas\""
