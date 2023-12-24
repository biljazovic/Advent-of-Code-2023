module Day22 (main22) where

import Util
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Linear.V3

type Brick = (V3 Int, V3 Int)

parse :: String -> Brick
parse str = (V3 x1 y1 z1, V3 x2 y2 z2)
  where
    [[x1,y1,z1], [x2, y2, z2]] = map (map read . splitOn ",") $ splitOn "~" str

intervals_intersect (x1, x2) (x1', x2') = not (xM < x'm || x'M < xm)
  where
    (xm, xM) = (min x1 x2, max x1 x2)
    (x'm, x'M) = (min x1' x2', max x1' x2')

supported ((x1, y1), (x2, y2), z) (V3 x1' y1' z1', V3 x2' y2' z2') =
  max z1' z2' == z - 1 && intervals_intersect (x1, x2) (x1', x2')
                                  && intervals_intersect (y1, y2) (y1', y2')

solveA bricks = listCount canDis bricks''
  where
    bricks' = sortOn (\(V3 _ _ z1, V3 _ _ z2)  -> min z1 z2) bricks
    bricks'' = go bricks' []
    suppCount (V3 x1 y1 z1, V3 x2 y2 z2) = listCount (supported ((x1, y1), (x2, y2), min z1 z2)) bricks''
    canDis b@(V3 x1 y1 z1, V3 x2 y2 z2) =
      let f b'@(V3 x1' y1' z1', V3 x2' y2' z2') =
            not (supported ((x1', y1'), (x2', y2'), min z1' z2') b) || suppCount b' > 1
       in all f bricks''
    go [] bs = bs
    go ((V3 x1 y1 z1, V3 x2 y2 z2) : rest) bs =
      let z = min z1 z2
          good z' = z' == 1 || any ( supported ((x1, y1), (x2, y2), z') ) bs
          zH = fromJust (find good (iterate (+(-1)) z))
          zDiff = z - zH
          b' = (V3 x1 y1 (z1 - zDiff), V3 x2 y2 (z2 - zDiff))
       in go rest (b' : bs)

invertMap :: Ord a => Ord b => Map a [b] -> Map b [a]
invertMap mapa = Map.fromListWith (++) assocs
  where
    assocs = map (\(k, v) -> (v, [k])) $ concatMap (\(k, vs) -> map (k, ) vs) $ Map.assocs mapa

solveB bricks = sum $ map ((+(-1)) . g) [0..length bricks - 1]
  where
    bricks' = sortOn (\(V3 _ _ z1, V3 _ _ z2)  -> min z1 z2) bricks
    bricks'' = go bricks' []
    suppGraph = Map.fromList $ map (\((V3 x1 y1 z1, V3 x2 y2 z2), i) -> (i, map snd $ filter (supported ((x1, y1), (x2, y2), min z1 z2) . fst) $ zip bricks'' [0..])) $ zip bricks'' [0..]
    suppGraphI = invertMap suppGraph
    g i = go' [i] suppGraph suppGraphI
    go' (toDel : rest) suppGraph' suppGraphI' =
      let next = Map.findWithDefault [] toDel suppGraphI'
          suppGraph'' = foldr (Map.adjust (filter (/= toDel))) suppGraph' next
          next' = filter (\n -> null (Map.findWithDefault [] n suppGraph'')) next
       in 1 + go' (rest ++ next') suppGraph'' (Map.delete toDel suppGraphI')
    go' [] _ _ = 0
    go [] bs = bs
    go ((V3 x1 y1 z1, V3 x2 y2 z2) : rest) bs =
      let z = min z1 z2
          good z' = z' == 1 || any ( supported ((x1, y1), (x2, y2), z') ) bs
          zH = fromJust (find good (iterate (+(-1)) z))
          zDiff = z - zH
          b' = (V3 x1 y1 (z1 - zDiff), V3 x2 y2 (z2 - zDiff))
       in go rest (b' : bs)

main22 :: IO ()
main22 = do
    input <- map parse . lines <$> readFile "res/input22"
    print $ solveA input
    print $ solveB input
