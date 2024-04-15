module Constants (module Constants) where

import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Picture (Color, Pictures, Text),
    scale,
    translate,
    rectangleSolid,
    orange,
    yellow,
    red,
    white,
    blue,
    magenta,
    cyan,
    green,

  )

data GameState = GameState
  { gameBoard :: [[Int]],
    cellSize :: Int,
    randomNumbers :: [Int]
  }  

windowSize :: (Int, Int)
windowSize = (620, 620)

windowPosition :: (Int, Int)
windowPosition = (100, 100)

windowCaption :: String
windowCaption = "2048"

windowPadding :: Int
windowPadding = 10

windowBackground :: Color
windowBackground = orange

boardPadding :: Int
boardPadding = 9

boardColor1 :: Color
boardColor1 = orange

boardColorLose :: Color
boardColorLose = red

boxSize :: Float
boxSize = 150.0

gameOverText :: Picture
gameOverText = scale 0.4 0.5 $ Color white $ Text "Game Over!"

cellColors :: [Color]
cellColors =
  [ white,
    yellow,
    red,
    blue,
    magenta,
    cyan,
    green
  ]
  