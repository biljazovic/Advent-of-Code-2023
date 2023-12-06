module Day06 (main06) where

import Util

solve :: Integer -> Integer -> Integer
solve t d = fromIntegral $ listCount good [0..t]
  where
    good x = (t-x) * x > d

solveA :: [Integer] -> [Integer] -> Integer
solveA times dists = product $ zipWith solve times dists

solveB :: [Integer] -> [Integer] -> Integer
solveB times dists = solveA [t] [d]
  where
    t = read $ foldr1 (++) $ map show times
    d = read $ foldr1 (++) $ map show dists

main06 :: IO ()
main06 = do
    (times : dists : _) <- map (map read . drop 1 . words) . lines <$> readFile "res/input06"
    print $ solveA times dists
    print $ solveB times dists
