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
  let rs = fromIntegral (4 * s + 5 * boardPadding)
      scoreText = scale 0.3 0.3 $ Color white $ Text ("Score: " ++ show currentScore ++ " Max Score: " ++ show maxScore)
   in Pictures $ drawBoard g s rs : [if checkWin g then drawWin s rs r else drawGameOver s rs r, translate (-rs / 2) (rs / 2 + 20) scoreText]

main :: IO ()
main = do
  content <- readFile "app/maxScore.txt"
  let maxScore = read content :: Int
  randomNumbers <- genRandomList
  let window = InWindow windowCaption windowSize windowPosition
      (initialBoard1, randomNumbers1, _) = updateBoard randomNumbers (replicate 4 [0, 0, 0, 0]) 0
      (initialBoard2, randomNumbers2, _) = updateBoard randomNumbers1 initialBoard1 0
      initialState = GameState initialBoard2 (calcCellSize windowSize) randomNumbers2 0 maxScore False maxScore
  finalScoreRef <- newIORef 0
  let handle' event state = do
        let newState = handle event state
        when (gameEnded newState) $ do
          putStrLn "Game ended, writing final score to file"
          writeIORef finalScoreRef (finalScore newState)
          finalScore <- readIORef finalScoreRef
          writeFile "app/maxScore.txt" (show finalScore)
        return $ newState {gameEnded = False}

  playIO window windowBackground 0 initialState (return . draw) handle' (\_ state -> return state)

genRandomList :: IO [Int]
genRandomList = randomRs (0, 15) <$> getStdGen
