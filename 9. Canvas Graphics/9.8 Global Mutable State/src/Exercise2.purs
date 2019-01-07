module Exercise2 where

import Prelude

import Effect
import Effect.Console
import Effect.Random
import Data.Array
import Data.Foldable
import Data.Maybe
import Graphics.Canvas
import Math as Math
import Data.Int
import Effect.DOM
import Color
import Exercise1 hiding (main)

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      node <- querySelector "#canvas"
      setStrokeStyle ctx "#000000"
      for_ node $ addEventListener "click" $ void do
        x <- random
        y <- random
        r <- random
        red <- random
        green <- random
        blue <- random
        setFillStyle ctx $ toHexString $ rgb' red green blue
        strokeAndFillPath ctx $ arc ctx
          { x: x * 600.0
          , y: y * 600.0
          , radius: r * 50.0
          , start: 0.0
          , end: 2.0 * Math.pi
          }
    Nothing -> error "Failed to retrieve HTML5 canvas with id \"canvas\""
