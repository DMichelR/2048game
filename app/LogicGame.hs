module LogicGame (module LogicGame) where

import Graphics.Gloss
import Constants
import Grids
import DrawBoard
import Data.List (transpose)
import Graphics.Gloss.Interface.Pure.Game
  ( 
    Display (InWindow),
    Event (EventKey, EventResize),
    Key (SpecialKey),
    KeyState (Down),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
    line,
    makeColor,
    pictures,
    play,
    
  )
import System.Random (Random (randomRs), getStdGen)

checkWin :: [[Int]] -> Bool
checkWin = any (elem winningPoint)

drawWin :: Int -> Float -> [a] -> Picture
drawWin boardSize cellSize gameOverList =
  if checkWin gameState
    then Pictures [drawWinRectangle boardColorWin (0, 0) (rectangleWidth, rectangleHeight), translate (calculatePosition 0 1) 0 winText]
    else Blank
  where
    gameState = [[winningPoint]]
    rectangleWidth = fromIntegral (4 * boardSize + 5 * boardPadding)
    rectangleHeight = rectangleWidth
    drawWinRectangle color (x, y) (width, height) = translate x y (Color color (rectangleSolid (toFloat width) (toFloat height)))
    toFloat = fromIntegral
    calculatePosition a k = toFloat (boardSize + boardPadding) * (toFloat a - k)

drawGameOver :: Int -> Float -> [a] -> Picture
drawGameOver boardSize cellSize gameOverList =
  if null gameOverList
    then Pictures [drawGameOverRectangle boardColorLose (0, 0) (rectangleWidth, rectangleHeight), translate (calculatePosition 0 1) 0 gameOverText]
    else Blank
  where
    rectangleWidth = fromIntegral (4 * boardSize + 5 * boardPadding)
    rectangleHeight = rectangleWidth
    drawGameOverRectangle color (x, y) (width, height) = translate x y (Color color (rectangleSolid (toFloat width) (toFloat height)))
    toFloat = fromIntegral
    calculatePosition a k = toFloat (boardSize + boardPadding) * (toFloat a - k)    

handle :: Event -> GameState -> GameState
handle (EventResize ns) (GameState g _ rs currentScore maxScore) = GameState g (calcCellSize ns) rs currentScore maxScore
handle (EventKey (SpecialKey KeyUp) Down _ _) g = updateGameState up g
handle (EventKey (SpecialKey KeyDown) Down _ _) g = updateGameState down g
handle (EventKey (SpecialKey KeyLeft) Down _ _) g = updateGameState left g
handle (EventKey (SpecialKey KeyRight) Down _ _) g = updateGameState right g
handle _ g = g

updateGameState :: ([[Int]] -> [[Int]]) -> GameState -> GameState
updateGameState moveFunction (GameState currentBoard cellSize randomNumbers currentScore maxScore)
  | upMoveResult == downMoveResult && downMoveResult == leftMoveResult && leftMoveResult == rightMoveResult
    = GameState updatedBoard cellSize [] currentScore (max maxScore currentScore)
  | currentBoard == updatedBoard
    = GameState updatedBoard cellSize randomNumbers currentScore maxScore
  | otherwise
    = let (newBoard, newRandomNumbers, newScore) = updateBoard randomNumbers updatedBoard currentScore
      in GameState newBoard cellSize newRandomNumbers newScore (max maxScore newScore)
  where
    updatedBoard = moveFunction currentBoard
    upMoveResult = up currentBoard
    downMoveResult = down currentBoard
    leftMoveResult = left currentBoard
    rightMoveResult = right currentBoard

updateBoard :: [Int] -> [[Int]] -> Int -> ([[Int]], [Int], Int)
updateBoard [] currentBoard currentScore = (currentBoard, [], currentScore)
updateBoard [_] currentBoard currentScore = (currentBoard, [], currentScore)
updateBoard (a : b : rest) currentBoard currentScore =
  case rowsAfter of
    [] -> (currentBoard, [], currentScore)
    (row : remainingRows) ->
      if row !! xPosition == 0
        then (rowsBefore ++ [newRow] ++ remainingRows, rest, currentScore + newValue)
        else updateBoard rest currentBoard currentScore
  where
    newValue = if a < 2 then 4 else 2
    xPosition = b `mod` 4
    yPosition = div b 4
    rowsBefore = take yPosition currentBoard
    rowsAfter = drop yPosition currentBoard
    newRow = take xPosition (head rowsAfter) ++ [newValue] ++ drop (xPosition + 1) (head rowsAfter)

up :: [[Int]] -> [[Int]]
up = transpose . right . transpose

down :: [[Int]] -> [[Int]]
down = transpose . left . transpose

right :: [[Int]] -> [[Int]]
right = map reverse . left . map reverse

merge :: [Int] -> [Int]
merge [] = []
merge [x] = [x]
merge (x:y:zs)
  | x == y = (x * 2) : merge zs
  | otherwise = x : merge (y : zs)

left :: [[Int]] -> [[Int]]
left = map moveLeft
  where
    moveLeft xs = take 4 $ merge $ filter (/= 0) xs ++ repeat 0
