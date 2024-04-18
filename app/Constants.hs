module Constants (module Constants) where
import Graphics.Gloss.Data.Color


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
windowBackground = makeColor (140/255) (195/255) (74/255) 1.0

boardPadding :: Int
boardPadding = 9

boardColor1 :: Color
boardColor1 = makeColor (140/255) (195/255) (74/255) 1.0

boardColorLose :: Color
boardColorLose = red

boxSize :: Float
boxSize = 150.0

gameOverText :: Picture
gameOverText = scale 0.4 0.5 $ Color white $ Text "Game Over!"

boardColorWin :: Color
boardColorWin = makeColor 0.10980392157 0.4862745098 0.8862745098 1.0

winText :: Picture
winText = scale 0.4 0.5 $ Color white $ Text "you win!"

winningPoint :: Int
winningPoint = 2046

cellColors :: [Color]
cellColors =
  [ white,
   makeColor (58/255) (160/255) (47/255) 1.0,
    makeColor (105/255) (191/255) (89/255) 1.0,
    makeColor (3/255) (187/255) (133/255) 1.0,
    makeColor (152/255) (223/255) (130/255) 1.0,
    makeColor (199/255) (255/255) (172/255) 1.0,
     makeColor (104/255) (221/255) (189/255) 1.0
  ]
