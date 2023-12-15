module Day12 (main12) where

import Util
import Data.Function.Memoize (memoFix2)

parse :: String -> (String, [Int])
parse = words >>> \(a:b:_) -> (a, map read (splitOn "," b))

solveA :: [(String, [Int])] -> Int
solveA = sum . map f
  where
    f (str, nums) = solveFast str nums

solveFast :: String -> [Int] -> Int
solveFast strA numsA = go' 0 0
  where
    lenA = length strA
    go' = memoFix2 $ \go pos posN ->
      let len = numsA !! posN
          s1 = if ch == '?' then go (pos+1) posN else 0
          ch = strA !! pos
          nullNums = posN == length numsA
       in if | pos == lenA -> if nullNums then 1 else 0
             | nullNums -> if any (== '#') (drop pos strA) then 0 else 1
             | lenA - pos < len -> 0
             | ch == '.' -> go (pos+1) posN
             | not (any (=='.') (map (strA !!) [pos..pos+len-1])) && (lenA - pos > len && (strA !! (pos + len) /= '#') || lenA - pos == len)
                  -> s1 + go (pos + len + 1) (posN+1)
             | otherwise -> s1

solveB :: [(String, [Int])] -> Int
solveB = sum . map (f . extend)
  where
    f (str, nums) = solveFast str nums
    extend (str, nums) = (intercalate "?" (replicate 5 str), concat $ replicate 5 nums)

main12 :: IO ()
main12 = do
    input <- map parse . lines <$> readFile "res/input12" :: IO [(String, [Int])]
    print $ solveA input
    print $ solveB input
