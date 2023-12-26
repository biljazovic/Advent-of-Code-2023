module Day19 (main19) where

import Data.List (find)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromJust, maybeToList)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Scanf

data Workflow = Workflow { _name :: String, _rules :: [Rule] }

type Part = [Int]

data Cond = Empty | More Char Int | Less Char Int

data Rule = CondRule { _cond :: Cond, _dest :: String }

rateInd :: Char -> Int
rateInd = \case
  'x' -> 0
  'm' -> 1
  'a' -> 2
  's' -> 3

partRating :: Part -> Char -> Int
partRating part r = part !! (rateInd r)

setRating :: Part -> Char -> Int -> Part
setRating part r n = take i part ++ [n] ++ drop (i+1) part
  where
    i = rateInd r

parseCond :: String -> Cond
parseCond (rate : cmp : rest) = case cmp of
                                  '>' -> More rate (read rest)
                                  '<' -> Less rate (read rest)

parseRule :: String -> Rule
parseRule str = case splitOn ":" str of
                  [dest] -> CondRule Empty dest
                  [cond, dest] -> CondRule (parseCond cond) dest

parseWorkflow :: String -> Workflow
parseWorkflow str = Workflow name (map parseRule rules)
  where
    (name : rules) = splitOneOf "{}," str

parsePart :: String -> Part
parsePart str = [x, m, a, s]
  where
    Just (x :+ m :+ a :+ s :+ ()) = scanf [fmt|{x=%d,m=%d,a=%d,s=%d}|] str

solveA (flows, parts) = sum . map sum . filter (flip good "in") $ parts
  where
    mapa :: Map String [Rule]
    mapa = Map.fromList $ map (\f -> (_name f, _rules f)) flows
    good part = \case
      "A" -> True
      "R" -> False
      str -> let rules = mapa Map.! str
              in good part $ _dest (fromJust $ find ((`passesRule` part) . _cond) rules)
    passesRule Empty _ = True
    passesRule (More r n) part = partRating part r > n
    passesRule (Less r n) part = partRating part r < n

type PartRange = (Part, Part)

partRangeCount :: PartRange -> Integer
partRangeCount (part1, part2) = product $ zipWith (\x1 x2 -> toInteger $ x2 - x1 + 1) part1 part2

solveB (flows, _) = sum . map (partRangeCount . snd) $ filter ((=="A") . fst) lF
  where
    mapa :: Map String [Rule]
    mapa = Map.fromList $ map (\fl -> (_name fl, _rules fl)) flows
    lF = go [("in", startRange)]
    go l = if all ((`elem` ["A", "R"]) . fst) l
              then l
              else go $ concatMap f l
    f p@(name, (part1, part2)) = if name `elem` ["A", "R"]
      then [p]
      else let rules = mapa Map.! name
               g rule partR (todo, done) = 
                 let (todoM, doneM) = splitRange partR rule
                  in (todo ++ maybeToList todoM, done ++ maybeToList doneM)
               go' (todo, done) (rule : rest) = go' (foldr (g rule) ([], done) todo) rest
               go' ([], done) [] = done
            in go' ([(part1, part2)], []) rules
    startRange = (replicate 4 1, replicate 4 4000)
    splitRange :: PartRange -> Rule -> (Maybe PartRange, Maybe (String, PartRange))
    splitRange partR@(part1, part2) (CondRule cond dest) = case cond of
      Empty -> (Nothing, Just (dest, partR))
      More r n -> let n1 = partRating part1 r
                      n2 = partRating part2 r
                   in if | n1 > n -> (Nothing, Just (dest, partR))
                         | n2 <= n -> (Just partR, Nothing)
                         | otherwise -> (Just (part1, setRating part2 r n), Just (dest, (setRating part1 r (n+1), part2)))
      Less r n -> let n1 = partRating part1 r
                      n2 = partRating part2 r
                   in if | n2 < n -> (Nothing, Just (dest, partR))
                         | n1 >= n -> (Just partR, Nothing)
                         | otherwise -> (Just (setRating part1 r n, part2), Just (dest, (part1, setRating part2 r (n-1))))

-- $> main19
main19 :: IO ()
main19 = do
    input <- (\[flows, parts] -> (map parseWorkflow (lines flows), map parsePart (lines parts))) . splitOn "\n\n" <$> readFile "res/input19"
    print $ solveA input
    print $ solveB input
