module Day01 (main01) where

import Util
import Data.Char
import Data.Maybe
import Control.Applicative

num_strs = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..]

toDigit :: Char -> Maybe Int
toDigit c = if isDigit c then Just (digitToInt c) else Nothing

solveA input = sum $ map f input
  where
    f str = let n1 = head $ mapMaybe toDigit str
                n2 = head $ mapMaybe toDigit $ reverse str
             in n1 * 10 + n2

solveB input = sum $ map f input
  where
    f str = let n1 = head $ mapMaybe (g head isPrefixOf) $ tails str
                n2 = head $ mapMaybe (g last isSuffixOf) $ map reverse (tails (reverse str))
             in n1 * 10 + n2
    g fun1 fun2 str =
      whenM (isDigit (fun1 str)) (digitToInt (fun1 str)) <|>
      asum [whenM (num_s `fun2` str) num | (num_s, num) <- num_strs]

-- $> main01
main01 :: IO ()
main01 = do
  input <- lines <$> readFile "res/input01"
  print $ solveA input
  print $ solveB input
