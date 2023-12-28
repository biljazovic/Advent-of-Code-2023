module Day08 (main08) where

import Util
import Data.Maybe (mapMaybe)
import Data.Digits (unDigits)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array ( Array )
import qualified Data.Array as Arr
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

parseDir str = (src, (left, right))
  where
    [src, rest] = splitOn " = " str
    left = take 3 (drop 1 rest)
    right = take 3 (drop 6 rest)

solveA (cycle -> instr, Map.fromList -> mapa) = go instr 0 "AAA"
  where
    go (ch : rest) d s = 
      let (left, right) = mapa Map.! s
          s' = if ch == 'L' then left else right
       in if s == "ZZZ" then d else go rest (d+1) s'

solveB (cycle -> instr, Map.fromList -> mapa) = foldr1 lcm $ map (go instr 0) starts
  where
    starts = filter ((=='A') . last) $ Map.keys mapa
    go (ch : rest) d s = 
      let (left, right) = mapa Map.! s
          s' = if ch == 'L' then left else right
       in if last s == 'Z' then d else go rest (d+1) s'

main08 :: IO ()
main08 = do
    input <- (\[instr, mapa] -> (instr, map parseDir $ lines mapa)) . splitOn "\n\n" <$> readFile "res/input08"
    print $ solveA input
    print $ solveB input
