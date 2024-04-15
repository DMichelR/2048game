module Grids (module Grids) where

import Constants
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Picture (Color, Pictures, Text),
    scale,
    translate,
    rectangleSolid,
  )

colorIndex :: Int -> Int
colorIndex n
  | n <= 1 = 0
  | otherwise = 1 + colorIndex (n `div` 2)

getColor :: Int -> Color
getColor i = cycle cellColors !! colorIndex i

drawRect :: Color -> (Float, Float) -> (Float, Float) -> Picture
drawRect c (x, y) (w, h) = translate x y (Color c (rectangleSolid (fromIntegral (round w)) (fromIntegral (round h))))

drawValue :: Int -> (Float, Float) -> Picture
drawValue value (x, y) = translate x y (scale 0.3 0.3 (Text (show value)))
