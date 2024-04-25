module Main (main) where

import Constants
import Control.Monad (when)
import Data.IORef
import Data.List (transpose)
import DrawBoard
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Event (EventKey, EventResize),
    Key (SpecialKey),
    KeyState (Down),
    Picture (Color, Pictures, Text),
  )
import Grids
import LogicGame
import System.IO
import System.Random (Random (randomRs), getStdGen)

draw :: GameState -> Picture
draw (GameState g s r currentScore maxScore ended finalScore) =
  let rs = fromIntegral (4 * s + 6 * boardPadding)
      scoreText = scale 0.3 0.3 $ Color (makeColor (184 / 255) (174 / 255) (161 / 255) 1.0) $ Text ("Score: " ++ show currentScore ++ " Max Score: " ++ show maxScore)
      button = translate 0 (-830) $ newGameButton s
   in Pictures $ drawBoard g s rs : [if checkWin g then drawWin s rs r else drawGameOver s rs r, translate (-rs / 2.32) (rs / 2 + 35) scoreText, button]

main :: IO ()
main = do
    content <- readFile "app/maxScore.txt"
    let maxScore = read content :: Int
    randomNumbers <- genRandomList
    let window = InWindow windowCaption windowSize windowPosition
        initialState = createInitialState randomNumbers maxScore
    finalScoreRef <- newIORef 0
    playIO window windowBackground 0 initialState (return . draw) (handleEvent finalScoreRef) updateState

createInitialState :: [Int] -> Int -> GameState
createInitialState randomNumbers score =
    let (initialBoard1, randomNumbers1) = updateBoard randomNumbers (replicate 4 (replicate 4 0)) 
        (initialBoard2, randomNumbers2) = updateBoard randomNumbers1 initialBoard1 
    in GameState initialBoard2 (calcCellSize windowSize) randomNumbers2 0 score False score

updateState :: Float -> GameState -> IO GameState
updateState _ = return
