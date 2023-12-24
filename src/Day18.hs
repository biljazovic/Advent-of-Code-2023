module Day18 (main18) where

import Util
import Data.Char (digitToInt)
import Data.Digits (unDigits)

solve xs = inter2 `div` 2 + ops
  where
    inter2 = abs area2 + 2 - ops
    ops = sum $ zipWith h points (tail points ++ [head points])
    area2 = sum $ zipWith g points (tail points ++ [head points])
    g (V2 x1 y1) (V2 x2 y2) = (x1+x2) * (y2-y1)
    h (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)
    points = scanl f (V2 0 0) xs
    f pos (toDir->(V2 dx dy), len) = pos + V2 (dx*len) (dy*len)
    toDir ch = case ch of
                'R' -> V2 0 1
                'D' -> V2 1 0
                'L' -> V2 0 (-1)
                'U' -> V2 (-1) 0


parseA :: String -> (Char, Int)
parseA str = (head chs, num)
  where
    chs : (read -> num) : _ = words str

parseB str = (dir, unDigits 16 (map digitToInt (take 5 rgb)))
  where
    rgb = init $ drop 2 (words str !! 2)
    dir = case last rgb of
            '0' -> 'R'
            '1' -> 'D'
            '2' -> 'L'
            '3' -> 'U'

main18 :: IO ()
main18 = do
    input <- lines <$> readFile "res/input18"
    print $ solve (map parseA input)
    print $ solve (map parseB input)
