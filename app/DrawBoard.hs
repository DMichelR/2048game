module DrawBoard (module DrawBoard) where
  
import Constants
import Grids
import Graphics.Gloss.Interface.Pure.Game
  ( Color,
    Picture (Color, Pictures, Text),
    scale,
    translate,
    rectangleSolid,
  )

drawEmptyCell :: Color -> (Float, Float) -> (Float, Float) -> Picture
drawEmptyCell backgroundColor (x, y) (w, h) = translate x y (Color backgroundColor (rectangleSolid w h))

drawNonEmptyCell :: Int -> (Float, Float) -> (Float, Float) -> Picture
drawNonEmptyCell value (x, y) (w, h) =
  Pictures [drawRect (getColor value) (x, y) (w, h), drawValue (fromIntegral value) (x, y)]

drawCell :: Int -> Int -> Int -> Int -> Float -> Picture
drawCell value x y size cellSize =
  if value == 0
    then drawEmptyCell (head cellColors) (cellPos x 1.5, cellPos y 1.5) (fromIntegral boardSize, fromIntegral boardSize)
    else drawNonEmptyCell value (cellPos x 1.5, cellPos y 1.5) (floatToInt boardSize, floatToInt boardSize)
  where
    floatToInt = fromIntegral
    drawRect c (x', y') (w, h) = translate x' y' (Color c (rectangleSolid (floatToInt w) (floatToInt h)))
    drawValue val (x', y') = translate (cellPos x 1.9) (cellPos y 1.7) (scale 0.4 0.5 (Text (show val)))
    cellPos a k = floatToInt (boardSize + boardPadding) * (floatToInt a - k)
    boardSize = fromIntegral (size + boardPadding)

calcCellSize :: (Int, Int) -> Int
calcCellSize (a, b) = fromIntegral ((min a b - 2 * windowPadding - 5 * boardPadding) `div` 4)

drawRectangle :: Color -> (Float, Float) -> (Float, Float) -> Picture
drawRectangle c (x, y) (w, h) = translate x y (Color c (rectangleSolid w h))

-- Función para dibujar el fondo del tablero
drawBoardBackground :: Int -> Float -> Picture
drawBoardBackground boardSize cellSize = drawRectangle boardColor1 (0, 0) (boardWidth, boardHeight)
  where
    boardWidth = fromIntegral (4 * boardSize + 5 * boardPadding)
    boardHeight = boardWidth

-- Función para dibujar el tablero del juego
drawBoard :: [[Int]] -> Int -> Float -> Picture
drawBoard g size cellSize =
  Pictures $
    drawBoardBackground size cellSize :
    concatMap (\(row, y) -> zipWith (\val x -> drawCell val x y size cellSize) row [0 ..]) (zip g [0 .. 3])
