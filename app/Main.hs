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
draw (GameState g s r) =
  let fi = fromIntegral
      rs = fromIntegral (4 * s + 5 * boardPadding)
   in Pictures $ drawBoard g s rs : [drawGameOver s rs r]

main :: IO ()
main = do
    randomNumbers <- genRandomList
    let window = InWindow windowCaption windowSize windowPosition
        (initialBoard1, randomNumbers1) = updateBoard randomNumbers $ replicate 4 [0, 0, 0, 0]
        (initialBoard2, randomNumbers2) = updateBoard randomNumbers1 initialBoard1
        initialState = GameState initialBoard2 (calcCellSize windowSize) randomNumbers2
    play window windowBackground 0 initialState draw handle (\_ state -> state)

genRandomList :: IO [Int]
genRandomList = randomRs (0, 15) <$> getStdGen
