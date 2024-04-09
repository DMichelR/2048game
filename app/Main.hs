module Main (main) where

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
window = InWindow "Basic Window" (800, 800) (100, 100)

drawGrid :: Picture -> Picture
drawGrid image = pictures [horizontalLines, verticalLines, translatedImage]
  where
    horizontalLines = pictures [line [(-400, y), (400, y)] | y <- [-400, -200 .. 400]]
    verticalLines = pictures [line [(x, -400), (x, 400)] | x <- [-400, -200 .. 400]]
    translatedImage = translate (-300) 300 image

updateImage :: Event -> Picture -> Picture
updateImage (EventKey (SpecialKey KeyUp) Down _ _) image = translate 0 200 image
updateImage (EventKey (SpecialKey KeyDown) Down _ _) image = translate 0 (-200) image
updateImage (EventKey (SpecialKey KeyLeft) Down _ _) image = translate (-200) 0 image
updateImage (EventKey (SpecialKey KeyRight) Down _ _) image = translate 200 0 image
updateImage _ image = image

main :: IO ()
main = play window white 60 initialImage drawGrid updateImage (\_ world -> world)
  where
    initialImage = color red $ rectangleSolid 200 200
