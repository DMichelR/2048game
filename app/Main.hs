module Main (main) where

import Graphics.Gloss
import Data.List (transpose)
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Display (InWindow),
    Event (EventKey, EventResize),
    Key (SpecialKey),
    KeyState (Down),
    Picture (Color, Pictures, Text)
  )
import System.Random (Random (randomRs), getStdGen)
import Constants
import Grids
import LogicGame
import DrawBoard

draw :: GameState -> Picture
draw (GameState g s r currentScore maxScore) =
  let rs = fromIntegral (4 * s + 5 * boardPadding)
      scoreText = scale 0.3 0.3 $ Color white $ Text ("Score: " ++ show currentScore ++ " Max Score: " ++ show maxScore)
   in Pictures $ drawBoard g s rs : [drawGameOver s rs r, translate (-rs / 2) (rs / 2 + 20) scoreText]

main :: IO ()
main = do
    randomNumbers <- genRandomList
    let window = InWindow windowCaption windowSize windowPosition
        (initialBoard1, randomNumbers1, _) = updateBoard randomNumbers (replicate 4 [0, 0, 0, 0]) 0
        (initialBoard2, randomNumbers2, _) = updateBoard randomNumbers1 initialBoard1 0
        initialState = GameState initialBoard2 (calcCellSize windowSize) randomNumbers2 0 0
    play window windowBackground 0 initialState draw handle (\_ state -> state)

genRandomList :: IO [Int]
genRandomList = randomRs (0, 15) <$> getStdGen
