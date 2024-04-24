module LogicGame (module LogicGame) where

import Constants
import Control.Monad (when)
import Data.List (transpose)
import DrawBoard
import Data.IORef
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
  ( Display (InWindow),
    Event (EventKey, EventResize),
    Key (SpecialKey),
    KeyState (Down),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeyUp),
    line,
    makeColor,
    pictures,
    play,
  )
import Grids
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
handle (EventResize ns) (GameState g cellSize rs currentScore maxScore ended finalScore) = GameState g (calcCellSize ns) rs currentScore maxScore ended finalScore
handle event gameState@(GameState board _ _ _ _ _ _) =
  if checkWin board
    then gameState {gameEnded = True, finalScore = max (currentScore gameState) (maxScore gameState)}
    else case event of
      EventKey (SpecialKey KeyUp) Down _ _ -> updateGameState up gameState
      EventKey (SpecialKey KeyDown) Down _ _ -> updateGameState down gameState
      EventKey (SpecialKey KeyLeft) Down _ _ -> updateGameState left gameState
      EventKey (SpecialKey KeyRight) Down _ _ -> updateGameState right gameState
      _ -> gameState

updateGameState :: ([[Int]] -> [[Int]]) -> GameState -> GameState
updateGameState moveFunction (GameState currentBoard cellSize randomNumbers currentScore maxScore ended finalScore)
  | upMoveResult == downMoveResult && downMoveResult == leftMoveResult && leftMoveResult == rightMoveResult =
      GameState updatedBoard cellSize [] currentScore (max maxScore currentScore) True (max currentScore maxScore)
  | currentBoard == updatedBoard =
      GameState updatedBoard cellSize randomNumbers currentScore maxScore ended finalScore
  | otherwise =
      let (newBoard, newRandomNumbers, newScore) = updateBoard randomNumbers updatedBoard currentScore
       in GameState newBoard cellSize newRandomNumbers newScore (max maxScore newScore) ended finalScore
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
merge (x : y : zs)
  | x == y = (x * 2) : merge zs
  | otherwise = x : merge (y : zs)

left :: [[Int]] -> [[Int]]
left = map moveLeft
  where
    moveLeft xs = take 4 $ merge $ filter (/= 0) xs ++ repeat 0


newGameButton :: Int -> Picture
newGameButton s = Pictures [button, buttonText]
  where
    rs = fromIntegral (4*s + 5*boardPadding)
    button = Color (makeColor 0.4 0.8 0.4 1) $ translate   300 (rs / 2 + 40)  $ rectangleSolid 140 60
    buttonText = Color black $ translate 250 (rs / 2 + 40) $ scale 0.15 0.15 $ Text "Restart Game"

newGame :: GameState -> IO GameState
newGame state = do
    randomNumbers <- genRandomList
    let (initialBoard1, randomNumbers1, _) = updateBoard randomNumbers (replicate 4 (replicate 4 0)) 0
        (initialBoard2, randomNumbers2, _) = updateBoard randomNumbers1 initialBoard1 0
    return $ state { gameBoard = initialBoard2, randomNumbers = randomNumbers2, currentScore = 0, gameEnded = False }



handleEvent :: IORef Int -> Event -> GameState -> IO GameState
handleEvent finalScoreRef event state = case event of
    EventKey (MouseButton LeftButton) Down _ (x, y)
        | x > 125 && x < 375 && y > 175 && y < 425 -> newGame state
    _ -> do
        let newState = handle event state
        when (gameEnded newState) $ do
            putStrLn "Juego finalizado, escribiendo puntuación final en el archivo"
            writeIORef finalScoreRef (finalScore newState)
        return newState

genRandomList :: IO [Int]
genRandomList = randomRs (0, 15) <$> getStdGen        