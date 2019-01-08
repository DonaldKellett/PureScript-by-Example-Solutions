module Exercise2 where

import Prelude

import Data.Maybe
import Data.Array
import Effect
import Graphics.Canvas
import Math as Math
import Effect.Console

lsystem :: forall a m s. Monad m =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s m = go (concatMap prod s) (m - 1)

data Alphabet = L | R | F

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
        initial = [F, R, R, F, R, R, F, R, R]
        productions :: Alphabet -> Sentence
        productions L = [L]
        productions R = [R]
        productions F = [F, L, F, R, R, F, L, F]
        interpret :: State -> Alphabet -> Effect State
        interpret state L = pure $ state { theta = state.theta - 5.0 * Math.pi / 12.0 }
        interpret state R = pure $ state { theta = state.theta + 5.0 * Math.pi / 12.0 }
        interpret state F = do
          let x = state.x + Math.cos state.theta * 25.0
              y = state.y + Math.sin state.theta * 25.0
          _ <- lineTo ctx x y
          pure { x, y, theta: state.theta }
        initialState :: State
        initialState = { x: 120.0, y: 200.0, theta: 0.0 }
      _ <- setStrokeStyle ctx "#000000"
      _ <- fillPath ctx do
        moveTo ctx initialState.x initialState.y
        _ <- lsystem initial productions interpret 3 initialState
        closePath ctx
      pure unit
    Nothing -> error "Failed to retrieve HTML5 canvas with id \"canvas\""
