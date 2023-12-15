module Day15 (main15) where

import Util
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Char (ord)

hash :: String -> Int
hash = foldl f 0
  where
    f s1 ch = (ord ch + s1) * 17 `mod` 256

solveA :: [String] -> Int
solveA = sum . map hash

solveB :: [String] -> Int
solveB = f . foldl g (IntMap.fromList (map (, []) [0..255]))
  where
    f mf = sum $ map i $ IntMap.assocs mf
    i (box_no, box) = sum $ zipWith (\(_, num) ind -> (box_no+1) * num * ind) box [1..]
    g :: IntMap [(String, Int)] -> String -> IntMap [(String, Int)]
    g m1 instr
      | last instr == '-' = let label = init instr
                                box_no = hash label
                                h = filter ((/= label) . fst)
                             in IntMap.adjust h box_no m1
      | otherwise = let [label, numS] = splitOn "=" instr
                        box_no = hash label
                        num = read numS
                        h [] = [(label, num)]
                        h ((label', num'):xs) = if label == label' then (label, num) : xs else (label',num') : h xs
                     in IntMap.adjust h box_no m1

main15 :: IO ()
main15 = do
    input <- splitOn "," . head . lines <$> readFile "res/input15"
    print $ solveA input
    print $ solveB input
