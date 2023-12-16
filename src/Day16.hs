module Day16 (main16) where

import Util
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Array as Arr
import Control.Parallel.Strategies (parMap, rdeepseq)

simulate :: (V2 Int, V2 Int) -> CharMatrix -> Int
simulate start mat = length $ nub $ Set.toList $ go Set.empty [start]
  where
    go :: Set (V2 Int, V2 Int) -> [(V2 Int, V2 Int)] -> Set (V2 Int)
    go seen [] = Set.map fst seen
    go seen (cur : rest) = if Set.member cur seen
      then go seen rest
      else let next = fst cur + snd cur
               new = if not $ inBounds (Arr.bounds mat) next
                        then []
                        else map (next, ) dirs'
               dir@(V2 dx dy) = snd cur
               dirs' = case mat Arr.! next of
                  '.' -> [dir]
                  '\\' -> [V2 dy dx]
                  '/' -> [V2 (-dy) (-dx)]
                  '-' -> if dx == 0 then [dir] else [V2 0 1, V2 0 (-1)]
                  '|' -> if dy == 0 then [dir] else [V2 1 0, V2 (-1) 0]
            in go (Set.insert cur seen) (new ++ rest)

solveA = (+(-1)) . simulate (V2 0 (-1), V2 0 1)

solveB mat = (-1) + maximum (parMap rdeepseq (\p -> simulate p mat) mogs)
  where
    (V2 _ _, V2 x1 y1) = Arr.bounds mat
    mogs = [(V2 (-1) y, V2 1 0) | y <- [0..y1]] ++
      [(V2 x (-1), V2 0 1) | x <- [0..x1]] ++
      [(V2 x (y1+1), V2 0 (-1)) | x <- [0..x1]] ++
      [(V2 (x1+1) y, V2 (-1) 0) | y <- [0..y1]]

main16 :: IO ()
main16 = do
    input <- parseMatrix <$> readFile "res/input16"
    print $ solveA input
    print $ solveB input
