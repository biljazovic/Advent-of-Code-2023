module Day10 (main10) where

import Data.Maybe (fromJust)
import qualified Data.Array as Arr
import Linear.V2
import Data.List
import Util (susedi4, parseMatrix)

susedi :: Char -> [V2 Int]
susedi = \case
  'F' -> [V2 0 1, V2 1 0]
  'J' -> [V2 0 (-1), V2 (-1) 0]
  '-' -> [V2 0 1, V2 0 (-1)]
  '|' -> [V2 1 0, V2 (-1) 0]
  'L' -> [V2 (-1) 0, V2 0 1]
  '7' -> [V2 0 (-1), V2 1 0]

loop mat = ret
  where
    startPos = fst $ fromJust $ find ((=='S') . snd) $ Arr.assocs mat
    susedStart = fromJust $ find (\p -> any ((== startPos) . (+ p)) (susedi (mat Arr.! p))) $ susedi4 (Just $ Arr.bounds mat) startPos
    ret = startPos : go susedStart startPos
    go curr prev = let next = head $ filter (/= prev) $ map (+ curr) $ susedi (mat Arr.! curr)
                    in if next == startPos then [curr] else curr : go next curr

-- logic copied from Day 18
solveB mat = inter2 `div` 2
  where
    inter2 = area2 + 2 - ops
    points = loop mat
    area2 = abs $ sum $ zipWith g points (tail points ++ [head points])
    g (V2 x1 y1) (V2 x2 y2) = (x1+x2) * (y2-y1)
    ops = sum $ zipWith h points (tail points ++ [head points])
    h (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

main10 :: IO ()
main10 = do
    input <- parseMatrix <$> readFile "res/input10"
    print $ length (loop input) `div` 2
    print $ solveB input
