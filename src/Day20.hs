module Day20 (main20) where

import Util
import Data.Maybe (maybeToList)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

type MyState = (Map String Bool, Map String Bool, [(String, String, Bool)], Int, Int, [Bool])
data Tip = FlipFlop | Conj | Broadcast
  deriving (Show, Eq)

process :: Map String (Tip, [String]) -> Map String [String] -> String -> MyState -> MyState
process _ _ _ s@(_, _, [], _, _, _) = s
process mapa inputs trackWho (flipflop, lastPulse, (source, dest, pulse) : rest, lowNum, highNum, track) = process mapa inputs trackWho (flipflop', lastPulse', rest ++ next, lowNum', highNum', track')
  where
    lastPulse' = Map.insert source pulse lastPulse
    instr = mapa Map.!? dest
    flipflop' = case instr of
                  Just (FlipFlop, _) -> if pulse then flipflop
                      else Map.alter (\case Nothing -> Just True; Just x -> Just (not x)) dest flipflop
                  _ -> flipflop
    next' = case instr of
              Nothing -> Nothing
              Just (tip, _) -> case tip of
               FlipFlop -> if pulse then Nothing
                                    else Just $ not $ Map.findWithDefault False dest flipflop
               Broadcast -> Just pulse
               Conj -> let ins = inputs Map.! dest
                        in Just $ not $ all (\x -> Map.findWithDefault False x lastPulse') ins
    next = case instr of
             Nothing -> []
             Just (_, dests) -> concatMap (\x -> map (dest,, x) dests) $ maybeToList next'
    lowNum' = if not pulse then lowNum + 1 else lowNum
    highNum' = if pulse then highNum + 1 else highNum
    track' = if dest == trackWho then track ++ [pulse] else track

parse :: String -> (Map String (Tip, [String]), Map String [String]) -> (Map String (Tip, [String]), Map String [String])
parse str (mapa, inputs) = (mapa', inputs')
  where
    [name', splitOn ", " -> dests] = splitOn " -> " str
    name = if head name' == 'b' then name' else tail name'
    inputs' = foldr (\d m -> Map.insertWith (++) d [name] m) inputs dests
    tip = case head name' of
            'b' -> Broadcast
            '%' -> FlipFlop
            '&' -> Conj
    mapa' = Map.insert name (tip, dests) mapa

solveA (mapa, inputs) = lowNum * highNum
  where
    (_, _, _, lowNum, highNum, _) = stateF
    stateF = iterate f (Map.empty, Map.empty, [], 0, 0, []) !! 1000
    f (x1, x2, [], x4, x5, x6) = process mapa inputs "" (x1, x2, [("button", "broadcaster", False)], x4, x5, x6)

trackOne (mapa, inputs) input who cnt = track
  where
    ((_, _, _, _, _, _), track) = stateF
    stateF = iterate f ((Map.empty, Map.empty, [], 0, 0, []), []) !! cnt
    f ((x1, x2, [], x4, x5, x6), xs) = 
      let (x1', x2', x3', x4', x5', x6') = process mapa inputs who (x1, x2, [("button", input, False)], x4, x5, [])
       in ((x1', x2', x3', x4', x5', x6'), xs ++ [and x6'])

compress :: [Bool] -> [(Bool, Int)]
compress = map (\g -> (head g, length g)) . group

-- was used to visualize the graph and extract input/output vertices for 4 distinct components
toDot mapa = let pairs = concatMap (\(src, (tip, dests)) -> map (\d -> (toName src, toName d)) dests) $ Map.toList mapa
                 names = Map.mapWithKey (\src (tip, dests) -> (show tip ++ "_" ++ src)) mapa
                 toName n = Map.findWithDefault n n names
              in unlines $ 
                ["digraph G {"] ++ map (\(src, dest) -> printf "%s -> %s;" src dest) pairs ++ ["}"]

solveB input = foldr1 lcm $ map (+1) [x1, x2, x3, x4]
  where
    x1 = snd $ head $ compress $ trackOne input "rf" "vq" 5000
    x2 = snd $ head $ compress $ trackOne input "lr" "tf" 5000
    x3 = snd $ head $ compress $ trackOne input "xh" "ln" 5000
    x4 = snd $ head $ compress $ trackOne input "km" "db" 5000

main20 :: IO ()
main20 = do
    input@(mapa, inputs) <- foldr parse (Map.empty, Map.empty) . lines <$> readFile "res/input20"
    print $ solveA input
    --putStr $ toDot mapa
    print $ solveB input
