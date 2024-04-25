import Test.QuickCheck
import Data.List (transpose)
import LogicGame (up, left, down, right)

board::[[Int]]
board = [
  [0, 0, 2, 0], 
  [0, 2, 2, 0], 
  [8, 0, 0, 0], 
  [0, 0, 0, 16]]

testMoveLeft :: Property
testMoveLeft = property $ left board === ([[2, 0, 0, 0], [4, 0, 0, 0], [8, 0, 0, 0], [16, 0, 0, 0]], 4)

testMoveRight :: Property
testMoveRight = property $ right board === ([[0, 0, 0, 2], [0, 0, 0, 4], [0, 0, 0, 8], [0, 0, 0, 16]], 4)

testMoveUp :: Property
testMoveUp = property $ down board === ([[8, 2, 4, 16], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 4)

testMoveDown :: Property
testMoveDown = property $ up board === ([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [8, 2, 4, 16]], 4)

main :: IO ()
main = do
    quickCheck testMoveLeft
    quickCheck testMoveRight
    quickCheck testMoveUp
    quickCheck testMoveDown
    