module Day05 (main05) where

import Util
import Control.Parallel.Strategies (parMap, rdeepseq)

data M = M { _dest :: Integer, _source :: Integer, _len :: Integer }
  deriving Show

data In = In {
  _seeds :: [Integer],
  _maps :: [[M]]
             } deriving Show

parse :: String -> In
parse ls = In seeds (map f maps')
  where
    seeds = map read (drop 1 (words seeds'))
    (seeds' : maps') = splitOn "\n\n" ls
    f m' = let ml = drop 1 $ lines m'
               in map (\l -> let ll = map read (words l) 
                              in M (ll !! 0) (ll !! 1) (ll !! 2)) ml

throughMap :: Integer -> [M] -> Integer
throughMap n maps = case find g maps of
                      Just (M dest src _) -> (n - src) + dest
                      Nothing -> n
  where
    g (M _ src len) = n >= src && n < src + len

solveA :: In -> Integer
solveA (In seeds maps) = minimum $ map f seeds
  where
    f seed = foldl throughMap seed maps

solveB (In seeds maps) = minimum $ parMap rdeepseq f (groupByn 2 seeds)
  where
    f (a : b : _) = minimum $ map (\s -> foldl throughMap s maps) [a..(a+b-1)]

main05 :: IO ()
main05 = do
    input <- parse <$> readFile "res/input05"
    print $ solveA input
    print $ solveB input
