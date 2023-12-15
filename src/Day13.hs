module Day13 (main13) where

import Util
import Data.Maybe (fromJust)

reflectionLinePos :: [String] -> Maybe Int
reflectionLinePos ls = find f [1..n-1]
  where
    n = length ls
    f pos = and $ zipWith (==) (reverse (take pos ls)) (drop pos ls)

solveA :: [[String]] -> Int
solveA lss = sum $ map f lss
  where
    f ls = let n1m = reflectionLinePos ls
               n2m = reflectionLinePos (transpose ls)
            in case n1m of
                 Just n1 -> 100 * n1
                 Nothing -> fromJust n2m

smudgedLinePos :: [String] -> Maybe Int
smudgedLinePos ls = find f [1..n-1]
  where
    n = length ls
    f pos = zipWith diffNums (reverse (take pos ls)) (drop pos ls)
              & sum & (==1)
    diffNums l1 l2 = zip l1 l2 & listCount (uncurry (/=))

solveB :: [[String]] -> Int
solveB lss = sum $ map f lss
  where
    f ls = let n1m = smudgedLinePos ls
               n2m = smudgedLinePos (transpose ls)
            in case n1m of
                 Just n1 -> 100 * n1
                 Nothing -> fromJust n2m

main13 :: IO ()
main13 = do
    input <- map lines . splitOn "\n\n" <$> readFile "res/input13"
    print $ solveA input
    print $ solveB input
