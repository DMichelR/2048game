module Constants (module Constants) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Picture (Color, Pictures, Text),
    rectangleSolid,
    scale,
    translate,
  )

data GameState = GameState
  { gameBoard :: [[Int]],
    cellSize :: Int,
    randomNumbers :: [Int],
    currentScore :: Int,
    maxScore :: Int,
    gameEnded :: Bool,
    finalScore :: Int
  }

windowSize :: (Int, Int)
windowSize = (900, 900)

windowPosition :: (Int, Int)
windowPosition = (100, 100)

windowCaption :: String
windowCaption = "CyberPunk 2048"

windowPadding :: Int
windowPadding = 110

windowBackground :: Color
windowBackground = makeColor (251 / 255) (248 / 255) (239 / 255) 1.0

boardPadding :: Int
boardPadding = 10

boardColor1 :: Color
boardColor1 = makeColor (184 / 255) (174 / 255) (161 / 255) 1.0

boardColorLose :: Color
boardColorLose = makeColor (158 / 255) (148 / 255) (138 / 255) 0.8

boxSize :: Float
boxSize = 150.0

gameOverText :: Picture
gameOverText = scale 0.5 0.6 $ Color white $ Text "Game Over!"

boardColorWin :: Color
boardColorWin = makeColor (158 / 255) (148 / 255) (138 / 255) 0.8

winText :: Picture
winText = scale 0.5 0.6 $ Color white $ Text "You Win!"

winningPoint :: Int
winningPoint = 2048

cellColors :: [Color]
cellColors =
  
  [ 
    -- cells
    makeColor (202 / 255) (193 / 255) (181 / 255) 1.0,
    -- 2
    makeColor (242 / 255) (208 / 255) (75 / 255) 1.0,
    -- 4
    makeColor (236 / 255) (196 / 255) (2 / 255) 1.0,
    -- 8
    makeColor (242 / 255) (177 / 255) (121 / 255) 1.0,
    -- 16
    makeColor (236 / 255) (141 / 255) (85 / 255) 1.0,
    -- 32
    makeColor (247 / 255) (123 / 255) (95 / 255) 1.0,
    -- 64
    makeColor (236 / 255) (89 / 255) (56 / 255) 1.0,
    -- 128
    makeColor (240 / 255) (102 / 255) (109 / 255) 1.0,
    -- 256
    makeColor (233 / 255) (81 / 255) (89 / 255) 1.0,
    -- 512
    makeColor (224 / 255) (67 / 255) (56 / 255) 1.0,
    -- 1024
    makeColor (96 / 255) (217 / 255) (146 / 255) 1.0,
    -- 2048
    makeColor (90 / 255) (230 / 255) (140 / 255) 1.0
  ]
