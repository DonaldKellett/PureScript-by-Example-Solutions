module Exercise1 where

import Prelude

import Effect
import Effect.Console
import Effect.Random
import Data.Array
import Data.Foldable
import Data.Maybe
import Graphics.Canvas
import Math as Math

strokeAndFillPath :: forall a. Context2D -> Effect a -> Effect a
strokeAndFillPath ctx path = do
  _ <- strokePath ctx path
  fillPath ctx path

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      setFillStyle ctx "#FF0000"
      setStrokeStyle ctx "#000000"
      for_ (1 .. 100) \_ -> do
        x <- random
        y <- random
        r <- random
        strokeAndFillPath ctx $ arc ctx
          { x: x * 600.0
          , y: y * 600.0
          , radius: r * 50.0
          , start: 0.0
          , end: 2.0 * Math.pi
          }
    Nothing -> error "Failed to retrieve HTML5 canvas with id \"canvas\""
