{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Day21 (main21) where

import Util
import Data.Maybe (fromJust)
import qualified Data.Array as Arr

floodCnt mat start len = length poss
  where
    poss = iterate step start !! len
    step = filter (\p -> (mat Arr.! p) /= '#') . mkUniq . concatMap (susedi4 (Just $ Arr.bounds mat))

findS mat = fst $ fromJust $ find ((=='S') . snd) $ Arr.assocs mat

solveA mat = floodCnt mat [findS mat] 64

solveB mat = sum [
    part_udlr mat (V2 xM (yM `div` 2)),
    part_udlr mat (V2 0 (yM `div` 2)),
    part_udlr mat (V2 (xM `div` 2) yM),
    part_udlr mat (V2 (xM `div` 2) 0),
    part_ur mat (V2 xM 0),
    part_ur mat (V2 xM yM),
    part_ur mat (V2 0 0),
    part_ur mat (V2 0 yM),
    part_s mat]
  where
    (_, V2 xM yM) = Arr.bounds mat

part_udlr mat pos = cnt_12 + floodCnt mat [pos] left3
  where
    (_, V2 xM yM) = Arr.bounds mat
    full_n = xM + 1
    s_to_u = full_n `div` 2
    s_to_d1 = s_to_u + 1
    d_to_ul = (xM `div` 2) + xM
    d1_to_d2 = xM + 1
    flood_1 = floodCnt mat [pos] (300 + ((steps_num - s_to_d1) `mod` 2))
    flood_2 = floodCnt mat [pos] (300 + ((steps_num - s_to_d1 - d1_to_d2) `mod` 2))
    n_gr = (steps_num - s_to_d1 - d_to_ul) `div` d1_to_d2
    cnt_12 = flood_1 * ((n_gr `div` 2) + 1) + flood_2 * ((n_gr+1) `div` 2)
    left3 = steps_num - s_to_d1 - (n_gr + 1) * d1_to_d2

part_ur mat pos = cnt_3 + cnt_4 + cnt_1 + cnt_2
  where
    s_to_ur = shortestPath mat (findS mat) (V2 0 yM)
    s_to_dl1 = s_to_ur + 2
    dl1_to_dl2 = xM + 1
    (_, V2 xM yM) = Arr.bounds mat
    dl_to_ur = xM + yM
    flood_1 = floodCnt mat [pos] (300 + ((steps_num - s_to_dl1) `mod` 2))
    flood_2 = floodCnt mat [pos] (300 + ((steps_num - s_to_dl1 - dl1_to_dl2) `mod` 2))
    n_gr = (steps_num - s_to_dl1 - dl_to_ur) `div` dl1_to_dl2
    left3 = steps_num - s_to_dl1 - (n_gr + 1) * dl1_to_dl2
    left4 = steps_num - s_to_dl1 - (n_gr + 2) * dl1_to_dl2
    flood_3 = floodCnt mat [pos] left3
    flood_4 = floodCnt mat [pos] left4
    cnt_3 = (n_gr + 2) * flood_3
    cnt_4 = (n_gr + 3) * flood_4
    n_gr_1 = n_gr - (n_gr `mod` 2)
    n_gr_2 = n_gr - 1 + (n_gr `mod` 2)
    cnt_1 = flood_1 * (((1 + (n_gr_1 + 1)) * ((n_gr_1 `div` 2) + 1)) `div` 2)
    cnt_2 = flood_2 * (((2 + (n_gr_2 + 1)) * ((n_gr_2 `div` 2) + 1)) `div` 2)

part_s mat = floodCnt mat [findS mat] (300 + (steps_num `mod` 2))

shortestPath :: CharMatrix -> V2 Int -> V2 Int -> Int
shortestPath mat start end = fromJust $ dijkstra [start] (== end) f
  where
    f pos = map (, 1) $ filter ((== '.') . (mat Arr.!)) $ susedi4 (Just $ Arr.bounds mat) pos

steps_num = 26501365

main21 :: IO ()
main21 = do
    input <- parseMatrix <$> readFile "res/input21"
    print $ solveA input
    print $ solveB input
