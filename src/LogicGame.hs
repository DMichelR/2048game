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
    rectangleWidth = fromIntegral (4 * boardSize + 7 * boardPadding)
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
    rectangleWidth = fromIntegral (4 * boardSize + 7 * boardPadding)
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

updateGameState :: ([[Int]] -> ([[Int]], Int)) -> GameState -> GameState
updateGameState moveFunction (GameState currentBoard cellSize randomNumbers currentScore maxScore ended finalScore)
  | fst upMoveResult == fst downMoveResult && fst downMoveResult == fst leftMoveResult && fst leftMoveResult == fst rightMoveResult =
      GameState updatedBoard cellSize [] currentScore (max maxScore currentScore) True (max currentScore maxScore)
  | currentBoard == updatedBoard =
      GameState updatedBoard cellSize randomNumbers currentScore maxScore ended finalScore
  | otherwise =
      let (newBoard, newRandomNumbers) = updateBoard randomNumbers updatedBoard
       in GameState newBoard cellSize newRandomNumbers newScore maxScore' ended finalScore
  where
    (updatedBoard, sum) = moveFunction currentBoard
    upMoveResult = up currentBoard
    downMoveResult = down currentBoard
    leftMoveResult = left currentBoard
    rightMoveResult = right currentBoard
    newScore = currentScore + sum
    maxScore' = max maxScore newScore 

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
    
up :: [[Int]] -> ([[Int]], Int)
up xs = let (boards, sumVal) = left $ map reverse $ transpose xs in (transpose $ map reverse boards, sumVal)

down :: [[Int]] -> ([[Int]], Int)
down xs = let (boards, sumVal) = right $ map reverse $ transpose xs in (transpose $ map reverse boards, sumVal)

right :: [[Int]] -> ([[Int]], Int)
right xs = let (boards, sumVal) = left $ map reverse xs in (map reverse boards, sumVal)

left :: [[Int]] -> ([[Int]], Int)
left xs = let (boards, sums) = unzip $ map moveLeft xs in (boards, sum sums)
    
merge :: [Int] -> Int -> ([Int], Int)
merge [] sumVal = ([], sumVal)
merge [x] sumVal = ([x], sumVal)
merge (x : y : zs) sumVal
  | x == y = let (merged, newSum) = merge zs (sumVal + x * 2) in ((x * 2) : merged, newSum)
  | otherwise = let (merged, newSum) = merge (y:zs) sumVal in (x : merged, newSum)

ensureFour :: [Int] -> [Int]
ensureFour xs = let filtered = filter (/= 0) xs
                    len = length filtered
                in  if len < 4 then filtered ++ replicate (4 - len) 0 else filtered

ensureFourRight :: [Int] -> [Int]
ensureFourRight xs = let len = length xs
                     in  if len < 4 then xs ++ replicate (4 - len) 0 else xs

moveLeft :: [Int] -> ([Int], Int)
moveLeft xs = let (merged, sumVal) = merge (ensureFour xs) 0 in (ensureFourRight merged, sumVal)

newGameButton :: Int -> Picture
newGameButton s = Pictures [border, button, buttonText]
  where
    rs = fromIntegral (4 * s + 6 * boardPadding)
    border = Color (makeColor (184 / 255) (174 / 255) (161 / 255) 1.0) $ translate 0 (rs / 2 + 80) $ rectangleSolid 160 60
    button = Color (makeColor (202 / 255) (193 / 255) (181 / 255) 1.0) $ translate   0 (rs / 2 + 80)  $ rectangleSolid 150 50
    buttonText = Color (makeColor (251 / 255) (248 / 255) (239 / 255) 1.0) $ translate (-68) (rs / 2 + 70) $ scale 0.16 0.16 $ Text "Restart Game"

newGame :: GameState -> IO GameState
newGame state = do
    randomNumbers <- genRandomList
    let (initialBoard1, randomNumbers1) = updateBoard randomNumbers (replicate 4 (replicate 4 0)) 
        (initialBoard2, randomNumbers2) = updateBoard randomNumbers1 initialBoard1 
    return $ state { gameBoard = initialBoard2, randomNumbers = randomNumbers2, currentScore = 0, gameEnded = False }

handleEvent :: IORef Int -> Event -> GameState -> IO GameState
handleEvent finalScoreRef event state = case event of
    EventKey (MouseButton LeftButton) Down _ (x, y)
        | x > (-76) && x < 76 && y > (-432) && y < (-382) -> newGame state
    _ -> do
        let newState = handle event state
        when (gameEnded newState) $ do
            writeIORef finalScoreRef (finalScore newState)
        return newState

genRandomList :: IO [Int]
genRandomList = randomRs (0, 15) <$> getStdGen
