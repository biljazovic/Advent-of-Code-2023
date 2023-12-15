module Day14 (main14) where

import Util

rollLeftOne :: String -> String
rollLeftOne = (\(str, saved) -> saved ++ str) . foldr f ("", "")
  where
    f ch (str, saved) = case ch of
      '.' -> ('.' : str, saved)
      '#' -> ('#' : (saved ++ str), "")
      'O' -> (str, 'O' : saved)

rollLeft :: [String] -> [String]
rollLeft = map rollLeftOne

rollUp = transpose . rollLeft . transpose

score :: [String] -> Int
score mat = sum $ zipWith f mat (reverse [1..(length mat)])
  where
    f str num = (listCount (=='O') str) * num

solveA :: [String] -> Int
solveA = score . rollUp

rotateRight :: [String] -> [String]
rotateRight = map reverse . transpose

cycle' = rollUp
  >>> rotateRight >>> rollUp 
  >>> rotateRight >>> rollUp 
  >>> rotateRight >>> rollUp
  >>> rotateRight

rollMany :: [String] -> [String]
rollMany = iterateWithCycle 1000000000 cycle'

solveB :: [String] -> Int
solveB = score . rollMany

main14 :: IO ()
main14 = do
    input <- lines <$> readFile "res/input14"
    print $ solveA input
    print $ solveB input
