module Main where

import Prelude

import Effect
import Effect.Console
import Data.Maybe
import Graphics.Canvas
import Math as Math
import Partial.Unsafe
import Web.HTML as Web.HTML
import Web.HTML.Window hiding (moveTo)
import Data.Array
import Data.Int

-- Boilerplate for Exercise 3
type Point = { x :: Number, y :: Number }

-- Exercise 3
renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx points = do
  case head points, tail points of
    Just p0, Just points' -> strokePath ctx $ do
      moveTo ctx p0.x p0.y
      foreachE points'
        (\pt -> lineTo ctx pt.x pt.y)
    _, _ -> pure unit

-- Arbitrary testing function for Exercise 3
f :: Number -> Point
f t =
  { x: 600.0 * t * t
  , y: 450.0 + 100.0 * t * Math.sin (20.0 * Math.pi * t)
  }

main :: Effect Unit
main = do
  window <- Web.HTML.window
  let canvasId = "canvas"
  maybeCanvas <- getCanvasElementById canvasId
  case maybeCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      -- Exercise 1
      setStrokeStyle ctx "#123456"
      strokePath ctx $ do
        moveTo ctx 100.0 50.0
        lineTo ctx 50.0 50.0
        lineTo ctx 50.0 100.0
        lineTo ctx 100.0 100.0
        moveTo ctx 50.0 100.0
        lineTo ctx 50.0 150.0
        lineTo ctx 100.0 150.0
        moveTo ctx 110.0 150.0
        lineTo ctx 150.0 100.0
        moveTo ctx 110.0 100.0
        lineTo ctx 150.0 150.0
        moveTo ctx 160.0 125.0
        lineTo ctx 210.0 125.0
        moveTo ctx
          (185.0 + 25.0 * Math.cos (Math.pi / 4.0))
          (125.0 + 25.0 * Math.sin (Math.pi / 4.0))
        arc ctx
          { x: 185.0
          , y: 125.0
          , radius: 25.0
          , start: -7.0 * Math.pi / 4.0
          , end: 0.0
          }
        moveTo ctx 220.0 100.0
        lineTo ctx 220.0 150.0
        moveTo ctx 220.0 110.0
        lineTo ctx 230.0 100.0
        lineTo ctx 250.0 100.0
        lineTo ctx 260.0 110.0
        moveTo ctx
          (295.0 + 25.0 * Math.cos (Math.pi / 4.0))
          (125.0 + 25.0 * Math.sin (Math.pi / 4.0))
        arc ctx
          { x: 295.0
          , y: 125.0
          , radius: 25.0
          , start: Math.pi / 4.0
          , end: 7.0 * Math.pi / 4.0
          }
        moveTo ctx 330.0 150.0
        lineTo ctx 330.0 100.0
        moveTo ctx 330.0 90.0
      setFillStyle ctx "#123456"
      fillPath ctx $ arc ctx
        { x: 330.0
        , y: 90.0
        , radius: 2.5
        , start: 0.0
        , end: 2.0 * Math.pi
        }
      strokePath ctx $ do
        moveTo ctx 390.0 110.0
        lineTo ctx 380.0 100.0
        lineTo ctx 360.0 100.0
        lineTo ctx 350.0 110.0
        lineTo ctx 390.0 140.0
        lineTo ctx 380.0 150.0
        lineTo ctx 360.0 150.0
        lineTo ctx 350.0 140.0
        moveTo ctx 400.0 125.0
        lineTo ctx 450.0 125.0
        moveTo ctx
          (425.0 + 25.0 * Math.cos (Math.pi / 4.0))
          (125.0 + 25.0 * Math.sin (Math.pi / 4.0))
        arc ctx
          { x: 425.0
          , y: 125.0
          , radius: 25.0
          , start: Math.pi / 4.0
          , end: 2.0 * Math.pi
          }
        moveTo ctx 480.0 70.0
        lineTo ctx 500.0 50.0
        lineTo ctx 500.0 150.0
        lineTo ctx 475.0 150.0
        moveTo ctx 500.0 150.0
        lineTo ctx 525.0 150.0
      -- Exercise 2
      setFillStyle ctx "#0000FF"
      fillPath ctx $ do
        moveTo ctx 50.0 200.0
        lineTo ctx 50.0 300.0
        lineTo ctx 100.0 300.0
        lineTo ctx 100.0 200.0
        closePath ctx
        moveTo ctx 150.0 200.0
        lineTo ctx 150.0 300.0
        lineTo ctx 200.0 300.0
        lineTo ctx 200.0 200.0
        closePath ctx
        moveTo ctx 400.0 250.0
        lineTo ctx 450.0 250.0
        arc ctx
          { x: 400.0
          , y: 250.0
          , radius: 50.0
          , start: 0.0
          , end: 11.0 * Math.pi / 6.0
          }
        closePath ctx
        moveTo ctx 425.0 244.0
        lineTo ctx
          (425.0 + 50.0 * Math.cos(-Math.pi / 6.0))
          (244.0 + 50.0 * Math.sin(-Math.pi / 6.0))
        arc ctx
          { x: 425.0
          , y: 244.0
          , radius: 50.0
          , start: -Math.pi / 6.0
          , end: 0.0
          }
        closePath ctx
      -- Exercise 3
      setStrokeStyle ctx "#FF0000"
      renderPath ctx
        (f <$> (\n -> toNumber n / 1000.0) <$> (0 .. 1000))
    Nothing -> do
      let errorMsg = "Failed to retrieve HTML5 canvas with id \"" <>
        canvasId <> "\""
      alert errorMsg window
      error errorMsg
