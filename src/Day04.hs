module Day04 (main04) where

import Util
import Text.ParserCombinators.ReadP

data Card = Card {
  _id :: Int,
  _sol :: [Int],
  _cands :: [Int]
} deriving Show

parseCard :: ReadP Card
parseCard = do
  id <- string "Card" *> skipSpaces *> parseInt <* char ':' <* skipSpaces
  sol <- sepBy1 parseInt skipSpaces <* skipSpaces <* char '|' <* skipSpaces
  cands <- sepBy1 parseInt skipSpaces <* skipSpaces
  return $ Card id sol cands

solveA :: [Card] -> Integer
solveA = sum . map f
  where
    f (Card _ sols cands) =
      let cnt = listCount (`elem` sols) cands
        in if cnt > 0 then 2^(cnt-1) else 0

solveB :: [Card] -> Integer
solveB cards = snd $ foldl g (repeat 1, 0) (map f cards)
  where
    g (l0 : rest, s0) n = let l1 = map (+l0) (take n rest) ++ drop n rest
                           in (l1, l0 + s0)
    f (Card _ sols cands) = listCount (`elem` sols) cands


main04 :: IO ()
main04 = do
    input <- lines <$> readFile "res/input04"
    let cards = map (fst . head . readP_to_S (parseCard <* eof)) input
    print $ solveA cards
    print $ solveB cards

