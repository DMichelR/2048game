module Poc (main) where

import Graphics.Gloss
  ( Display (InWindow),
    Picture,
    color,
    line,
    pictures,
    play,
    rectangleSolid,
    red,
    translate,
    white,
  )
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))

window :: Display
window = InWindow "Basic Window" (800, 800) (10, 10)

boxSize :: Float
boxSize = 100.0

numRowsAndCols :: Int
numRowsAndCols = 4

horizontalLine :: Float -> Picture
horizontalLine y = line [(-200 , y), (200 , y)]

verticalLine :: Float -> Picture
verticalLine x = line [(x, -200 ), (x, 200)]

drawGrid :: Picture
drawGrid = pictures [horizontalLines, verticalLines,borderLines]
  where
    horizontalLines = pictures [horizontalLine (fromIntegral y * boxSize - 200) | y <- [0 .. numRowsAndCols]]
    verticalLines = pictures [verticalLine (fromIntegral x * boxSize - 200) | x <- [0 .. numRowsAndCols]]
    borderLines = pictures [line [(-200, -200), (-200, 200)],  
                            line [(200, -200), (200, 200)],    
                            line [(-200, 200), (200, 200)],   
                            line [(-200, -200), (200, -200)]]  

drawRed :: (Int, Int) -> Picture
drawRed (row, col) = translate x y $ color red $ rectangleSolid boxSize boxSize
  where
    x = fromIntegral col * boxSize - 200 + boxSize / 2
    y = 200 - (fromIntegral row * boxSize) - boxSize / 2

newtype GameState = GameState
  { playerPositionRed :: (Int, Int)}

initialPositionRedCoord :: (Int, Int)
initialPositionRedCoord = (0, 0)

updateImage :: Event -> GameState -> GameState
updateImage (EventKey (SpecialKey KeyUp) Down _ _) gameState = movePlayer (-1, 0) gameState
updateImage (EventKey (SpecialKey KeyDown) Down _ _) gameState = movePlayer (1, 0) gameState
updateImage (EventKey (SpecialKey KeyLeft) Down _ _) gameState = movePlayer (0, -1) gameState
updateImage (EventKey (SpecialKey KeyRight) Down _ _) gameState = movePlayer (0, 1) gameState
updateImage _ gameState = gameState

movePlayer :: (Int, Int) -> GameState -> GameState
movePlayer (dx, dy) gameState@GameState { playerPositionRed = (row, col) } =
  gameState { playerPositionRed = (newRow, newCol) }
  where
    newRow = boundValue 0 (numRowsAndCols - 1) (row + dx)
    newCol = boundValue 0 (numRowsAndCols - 1) (col + dy)

boundValue :: Int -> Int -> Int -> Int
boundValue minVal maxVal val
  | val < minVal = minVal
  | val > maxVal = maxVal
  | otherwise    = val

draw :: GameState -> Picture
draw gameState = pictures [drawGrid, drawRed (playerPositionRed gameState)]

main :: IO ()
main = play window white 60 initialImage draw updateImage (\_ gameState -> gameState)
  where
    initialImage = GameState { playerPositionRed = initialPositionRedCoord}
