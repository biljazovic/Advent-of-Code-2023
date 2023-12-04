module Day03 (main03) where

import Util
import Data.Digits (unDigits)
import qualified Data.Array as Arr
import Data.Char (isDigit, digitToInt)

nums :: CharMatrix -> [[(Char, V2 Int)]]
nums mat = concatMap f rows
  where
    f = filter (all (isDigit . fst)) . groupBy (\(c1,_) (c2,_) -> isDigit c1 == isDigit c2)
    rows = [ [ (mat Arr.! V2 (fromIntegral i) (fromIntegral j), V2 i j) | j <- [0..j1] ] | i <- [0..i1] ]
    (_, V2 i1 j1) = Arr.bounds mat

toNum :: [(Char, V2 Int)] -> Int
toNum = unDigits 10 . map (digitToInt . fst)

solveA mat = sum $ map toNum $ filter good (nums mat)
  where
    good = any (adjSymbol . snd)
    adjSymbol p = any (isSymbol . (Arr.!) mat) (susedi8 (Just $ Arr.bounds mat) p)
    isSymbol ch = ch /= '.' && not (isDigit ch)

solveB mat = map (\p -> filter (adjNum p) (nums mat)) gears
            & filter (length >>> (==2))
            & map (map toNum >>> product)
            & sum
  where
    adjNum p num = elem p $ concatMap (susedi8 (Just $ Arr.bounds mat) . snd) num
    gears = Arr.assocs mat & filter (snd >>> (=='*')) & map fst

main03 :: IO ()
main03 = do
    input <- readFile "res/input03"
    let mat = parseMatrix input
    print $ solveA mat
    print $ solveB mat
