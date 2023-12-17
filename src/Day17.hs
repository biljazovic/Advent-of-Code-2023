module Day17 (main17) where

import Util
import qualified Data.Array as Arr
import Data.Char (digitToInt)

solveA :: CharMatrix -> Maybe Int
solveA mat = dijkstra start goal neighs
  where
    bnds@(V2 _ _, V2 x1 y1) = Arr.bounds mat
    start = [(V2 0 0, V2 0 1, 0)] :: [(V2 Int, V2 Int, Int)]
    goal (pos, dir, straight_num) = pos == V2 x1 y1
    neighs (pos, dir@(V2 dx dy), straight_num) =
      let good (pos', dir', straight_num') = inBounds bnds pos' && straight_num' <= 3
          dirL = V2 (-dy) (-dx)
          dirR = V2 dy dx
          cands = [(pos + dir, dir, straight_num + 1),
                   (pos + dirL, dirL, 1),
                   (pos + dirR, dirR, 1)]
       in map (\t@(p, _, _) -> (t, digitToInt (mat Arr.! p))) $ filter good cands

solveB :: CharMatrix -> Maybe Int
solveB mat = dijkstra start goal neighs
  where
    bnds@(V2 _ _, V2 x1 y1) = Arr.bounds mat
    start = [(V2 0 0, V2 0 1, 0)] :: [(V2 Int, V2 Int, Int)]
    goal (pos, dir, straight_num) = pos == V2 x1 y1 && straight_num >= 4
    neighs (pos, dir@(V2 dx dy), straight_num) =
      let good (pos', dir', straight_num') = inBounds bnds pos' && straight_num' <= 10 && (dir' == dir || straight_num >= 4)
          dirL = V2 (-dy) (-dx)
          dirR = V2 dy dx
          cands = [(pos + dir, dir, straight_num + 1),
                   (pos + dirL, dirL, 1),
                   (pos + dirR, dirR, 1)]
       in map (\t@(p, _, _) -> (t, digitToInt (mat Arr.! p))) $ filter good cands

main17 :: IO ()
main17 = do
    input <- parseMatrix <$> readFile "res/input17"
    print $ solveA input
    print $ solveB input
