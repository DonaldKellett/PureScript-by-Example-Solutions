module Exercise5 where

import Prelude

import Data.Maybe
import Data.Array hiding (foldM)
import Effect
import Graphics.Canvas
import Math as Math
import Effect.Console
import Data.Foldable

iterM :: forall a m. Monad m => (a -> m a) -> Int -> m a -> m a
iterM _ 0 x = x
iterM f n x = do
  x' <- iterM f (n - 1) x
  f x'

lsystem :: forall a f m s. Monad m => Monad f => Foldable f =>
  f a -> (a -> f a) -> (s -> a -> m s) -> Int -> s -> m s
lsystem init prod interpret n state =
  foldM interpret state $ iterM prod n init

type Angle = Number

data Alphabet = L Angle | R Angle | F

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Angle
  }

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      let
        initial :: Sentence
        initial =
          [ F
          , R (Math.pi / 3.0)
          , R (5.0 * Math.pi / 12.0)
          , F
          , R (Math.pi / 3.0)
          , R (Math.pi / 3.0)
          , F
          , R (Math.pi / 4.0)
          , R (Math.pi / 3.0)
          ]
        productions :: Alphabet -> Sentence
        productions (L theta) =
          [L (Math.remainder (theta + Math.pi / 12.0) (Math.pi / 2.0))]
        productions (R theta) =
          [R (Math.remainder (theta + Math.pi / 12.0) (Math.pi / 2.0))]
        productions F =
          [ F
          , L (5.0 * Math.pi / 12.0)
          , F
          , R (Math.pi / 4.0)
          , R (5.0 * Math.pi / 12.0)
          , F
          , L (Math.pi / 4.0)
          , F
          ]
        interpret :: State -> Alphabet -> Effect State
        interpret state (L angle) = pure $ state { theta = state.theta - angle }
        interpret state (R angle) = pure $ state { theta = state.theta + angle }
        interpret state F = do
          let x = state.x + Math.cos state.theta * 2.75
              y = state.y + Math.sin state.theta * 2.75
          _ <- lineTo ctx x y
          pure { x, y, theta: state.theta }
        initialState :: State
        initialState = { x: 200.0, y: 25.0, theta: 0.0 }
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
