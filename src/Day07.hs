module Day07 (main07) where

import Util
import Data.Maybe (fromJust)

cards = "AKQT98765432J"

cardStrength :: Char -> Int
cardStrength ch = length cards - fromJust (ch `elemIndex` cards)


compareHands :: (String, String) -> (String, String) -> Ordering
compareHands (h1J, h1) (h2J, h2) = if h1S /= h2S
                        then compare h1S h2S
                        else compare (map cardStrength h1) (map cardStrength h2)
  where
    h1S = handStrength $ reverse $ sortOn length $ group $ sort h1J
    h2S = handStrength $ reverse $ sortOn length $ group $ sort h2J
    handStrength xs
      | length xs == 1 = 7            -- five of a kind
      | length (head xs) == 4 = 6     -- four of a kind
      | length xs == 2 = 5            -- full house
      | length (head xs) == 3 = 4     -- three of a kind
      | length xs == 3 = 3            -- two pair
      | length xs == 4 = 2            -- one pair
      | otherwise = 1                 -- high card


solveA :: [(String, Int)] -> Int
solveA input = sum $ zipWith (\(_, s) i -> s*i) sortedInput [1..]
  where
    sortedInput = sortBy (\(h1, _) (h2, _) -> compareHands (h1, h1) (h2, h2)) input

joker :: String -> String
joker h = let gh = reverse $ sortOn length $ group $ sort h
              jTo
                | head (head gh) == 'J' && length gh == 1 = 'J'
                | head (head gh) == 'J' = head (gh !! 1)
                | otherwise = head (head gh)
           in map (\ch -> if ch == 'J' then jTo else ch) h

solveB :: [(String, Int)] -> Int
solveB input = sum $ zipWith (\(_, s) i -> s*i) sortedInput [1..]
  where
    sortedInput = sortBy (\(h1, _) (h2, _) -> compareHands (joker h1, h1) (joker h2, h2)) input

-- $> main07
main07 :: IO ()
main07 = do
    input <- map ((\(a : b : _) -> (a, read b :: Int)) . words) . lines <$> readFile "res/input07"
    print $ solveA input
    print $ solveB input
