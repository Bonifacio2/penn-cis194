{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise1

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.0

trafficLight :: [Bool] -> Picture
trafficLight [_, True, True]  = topCircle red  & midCircle yellow & botCircle black & frame
trafficLight [True, _, _]     = topCircle black & midCircle black & botCircle green & frame
trafficLight [_, True, _]     = topCircle black & midCircle yellow & botCircle black & frame
trafficLight [_, _, True]     = topCircle red & midCircle black & botCircle black & frame

trafficController :: Double -> Picture
trafficController t
  | (round(t) `mod` 8) `elem` [0, 1, 2] = trafficLight [True, False, False]
  | (round(t) `mod` 8) == 3             = trafficLight [False, True, False]
  | (round(t) `mod` 8) `elem` [4, 5, 6] = trafficLight [False, False, True]
  | otherwise                           = trafficLight [False, True, True]

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2
main :: IO ()
main = exercise2

tree :: Integer -> Double -> Picture
tree 0 petalSize = colored yellow (solidCircle petalSize)
tree n 0 = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) 0) & rotated (- pi/10) (tree (n-1) 0))
tree n petalSize = translated 0 1 (
  rotated (pi/10) (tree (n-1) petalSize) & rotated (- pi/10) (tree (n-1) petalSize))
  & path [(0,0),(0,1)]
  

treeBlossomController :: Double -> Picture
treeBlossomController time
  | time < 10 = tree 8 (time / 50) 
  | otherwise        = tree 8 0.2
exercise2 :: IO ()
exercise2 = animationOf treeBlossomController

-- Exercise 3

wall, ground, storage, box, emptyTile :: Picture
wall =    solidRectangle 1 1
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.2
box =     colored orange (solidRectangle 1 1)
emptyTile = colored black (solidRectangle 1 1)


drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = emptyTile

possibleTiles :: [(Int, Int)]
possibleTiles = [tile | tile <- [(x, y) | x <- [-10..10], y <- [-10..10]]]

pics :: [Picture]
pics = [translated (fst tile) (snd tile) (drawTile (maze (fst tile) (snd tile))) | tile <- possibleTiles]
--[ maze (fst tile) (snd tile)  | tile <- possibleTiles]

--concatPics :: [Picture] -> Picture
--concatPics [x:[]] = x
--concatPics (x:xs) = x 

pictureOfMaze :: Picture
pictureOfMaze = head pics

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 
