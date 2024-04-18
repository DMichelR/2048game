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
handle (EventResize ns) (GameState g _ rs) = GameState g (calcCellSize ns) rs
handle event gameState =
  if checkWin (gameBoard gameState)
    then gameState
    else case event of
      EventKey (SpecialKey KeyUp) Down _ _ -> updateGameState up gameState
      EventKey (SpecialKey KeyDown) Down _ _ -> updateGameState down gameState
      EventKey (SpecialKey KeyLeft) Down _ _ -> updateGameState left gameState
      EventKey (SpecialKey KeyRight) Down _ _ -> updateGameState right gameState
      _ -> gameState

updateGameState :: ([[Int]] -> [[Int]]) -> GameState -> GameState
updateGameState moveFunction (GameState currentBoard cellSize randomNumbers)
  | upMoveResult == downMoveResult && downMoveResult == leftMoveResult && leftMoveResult == rightMoveResult
    = GameState updatedBoard cellSize []
  | currentBoard == updatedBoard
    = GameState updatedBoard cellSize randomNumbers
  | otherwise
    = let (newBoard, newRandomNumbers) = updateBoard randomNumbers updatedBoard
      in GameState newBoard cellSize newRandomNumbers
  where
    updatedBoard = moveFunction currentBoard
    upMoveResult = up currentBoard
    downMoveResult = down currentBoard
    leftMoveResult = left currentBoard
    rightMoveResult = right currentBoard

updateBoard :: [Int] -> [[Int]] -> ([[Int]], [Int])
updateBoard [] currentBoard = (currentBoard, [])
updateBoard [_] currentBoard = (currentBoard, [])
updateBoard (a : b : rest) currentBoard =
  case rowsAfter of
    [] -> (currentBoard, [])
    (row : remainingRows) ->
      if row !! xPosition == 0
        then (rowsBefore ++ [newRow] ++ remainingRows, rest)
        else updateBoard rest currentBoard
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
