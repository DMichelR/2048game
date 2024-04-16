module Grids (module Grids) where

import Constants
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Picture (Color, Pictures, Text),
    rectangleSolid,
    scale,
    translate,
  )

colorIndex :: Int -> Int
colorIndex n
  | n <= 1 = 0
  | otherwise = 1 + colorIndex (n `div` 2)

getColor :: Int -> Color
getColor i = cycle cellColors !! colorIndex i

drawRect :: Color -> (Float, Float) -> (Float, Float) -> Picture
drawRect c (x, y) (w, h) = translate x y (Color c (rectangleSolid (fromIntegral (round w)) (fromIntegral (round h))))

drawValue :: Int -> (Float, Float) -> Float -> Picture
drawValue value (x, y) cellSize = translate x y (scale (cellSizeRatio) (cellSizeRatio) (Text (show value)))
  where
    cellSizeRatio = (cellSize / 797) * 0.5
