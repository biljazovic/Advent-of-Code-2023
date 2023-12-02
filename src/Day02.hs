module Day02 (main02) where

import Util
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP

data Color = Blue | Red | Green
  deriving (Show, Eq, Ord)
data Game = Game { _id :: Int , _colss :: [[(Color, Int)]] }
  deriving Show

parseColor :: ReadP Color
parseColor = choice (map f [(Blue, "blue"), (Red, "red"), (Green, "green")])
  where
    f (color, color_s) = string color_s $> color

parseIntColor :: ReadP (Color, Int)
parseIntColor = do
  n <- parseInt <* space
  c <- parseColor
  return (c, n)

parseGame :: ReadP Game
parseGame = do
  id <- string "Game " *> parseInt
  ss <- string ": " *> sepBy1 (sepBy1 parseIntColor (string ", ")) (string "; ")
  return $ Game id ss

countCol :: Color -> [(Color, Int)] -> Int
countCol c ci = Map.findWithDefault 0 c (Map.fromList ci)

solveA = sum . map _id . filter f
  where
    f (Game _ colss) = all (g colss) [(Red, 12), (Green, 13), (Blue, 14)]
    g colss (c, n) = maximum (map (countCol c) colss) <= n

solveB = sum . map f
  where
    f (Game _ colss) = product $ map (g colss) [Red, Green, Blue]
    g colss c = maximum (map (countCol c) colss)

-- $> main02
main02 :: IO ()
main02 = do
    input <- lines <$> readFile "res/input02"
    let games = map (fst . head . readP_to_S (parseGame <* eof)) input
    print $ solveA games
    print $ solveB games
