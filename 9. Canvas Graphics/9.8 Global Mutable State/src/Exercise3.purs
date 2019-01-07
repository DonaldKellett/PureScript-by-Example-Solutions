module Exercise3 where

import Prelude

import Data.Foldable
import Data.Int
import Data.Maybe
import Effect
import Effect.Console
import Effect.DOM
import Effect.Ref
import Graphics.Canvas
import Math as Math

translate'
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate' dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

rotateScene :: Context2D -> Number -> Number -> Number -> Effect Unit
rotateScene ctx x y theta = do
  translate ctx { translateX: x, translateY: y }
  rotate ctx theta
  translate ctx { translateX: -x, translateY: -y }

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      rotateScene ctx 300.0 300.0 (3.0 * Math.pi / 2.0)
      _ <- setFillStyle ctx "#0000FF"
      _ <- fillPath ctx $ rect ctx $ translate' (-200.0) (-200.0)
        { x: 250.0
        , y: 250.0
        , width: 100.0
        , height: 100.0
        }
      _ <- setFillStyle ctx "#00FF00"
      _ <- fillPath ctx $ arc ctx $ translate' 200.0 200.0
        { x: 300.0
        , y: 300.0
        , radius: 50.0
        , start: Math.pi * 5.0 / 8.0
        , end: Math.pi * 2.0
        }
      _ <- setFillStyle ctx "#FF0000"
      fillPath ctx $ do
        _ <- moveTo ctx 300.0 260.0
        _ <- lineTo ctx 260.0 340.0
        _ <- lineTo ctx 340.0 340.0
        closePath ctx
    Nothing -> error "Failed to retrieve HTML5 canvas with id \"canvas\""
