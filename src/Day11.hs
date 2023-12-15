module Day11 (main11) where

import Util

solve mult mat = sum lens
  where
    is = scanl f 0 mat
    js = scanl f 0 (transpose mat)
    f prev str = if all (== '.') str then prev + mult else prev + 1
    poss = [ (i, j) | (str, i) <- zip mat is, (ch, j) <- zip str js, ch == '#' ]
    lens = map (\((p1x, p1y), (p2x, p2y)) -> abs (p1x-p2x) + abs (p1y-p2y)) (makePairs poss)

main11 :: IO ()
main11 = do
    input <- lines <$> readFile "res/input11"
    print $ solve 2 input
    print $ solve 1000000 input
