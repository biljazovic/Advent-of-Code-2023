module Day09 (main09) where

import Util

solveA :: [[Int]] -> Int
solveA = sum . map f
  where
    f hst0 = foldr h 0 $ iterateUntil (snd >>> all (== 0)) g hst0
    h xs num = num + last xs
    g xs = zipWith (-) (tail xs) xs

solveB :: [[Int]] -> Int
solveB = sum . map f
  where
    f hst0 = foldr h 0 $ iterateUntil (snd >>> all (== 0)) g hst0
    h xs num = head xs - num
    g xs = zipWith (-) (tail xs) xs

main09 :: IO ()
main09 = do
    input <- map (map read . words) . lines <$> readFile "res/input09"
    print $ solveA input
    print $ solveB input
