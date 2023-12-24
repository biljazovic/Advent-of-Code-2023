module Day24 (main24) where

import Util
import Linear.V3
import Text.Printf

minXY = 200000000000000
maxXY = 400000000000000

parse :: String -> (V3 Int, V3 Int)
parse str = (V3 x y z, V3 vx vy vz)
  where
    [[x, y, z], [vx, vy, vz]] = map (map read . splitOn ", ") (splitOn " @ " str)

intersection :: (V3 Int, V3 Int) -> (V3 Int, V3 Int) -> (Double, Double)
intersection (V3 x1 y1 _, V3 vx1 vy1 _) (V3 x2 y2 _, V3 vx2 vy2 _) = if s > 0 && t > 0 then (xi, yi) else (1/0, 1/0)
  where
    s = fromIntegral (vx1*(y2-y1) + vy1*(x1-x2)) / fromIntegral (vx2*vy1 - vx1*vy2)
    t = fromIntegral (vx2*(y1-y2) + vy2*(x2-x1)) / fromIntegral (vx1*vy2 - vx2*vy1)
    xi = fromIntegral x2 + (s * fromIntegral vx2)
    yi = fromIntegral y2 + (s * fromIntegral vy2)


solveA input = listCount good $ makePairs input
  where
    good (p1, p2) = let (xi, yi) = intersection p1 p2
                     in all (\i -> i >= minXY && i <= maxXY) [xi, yi]

-- outputs system of equations, answer is x+y+z, evaluated in SageMath:
-- var('x y z vx vy vz t1 t2 t0')
-- solve([ <equations> ],x,y,z,vx,vy,vz,t1,t2,t0)
solveB input = intercalate ",\n" eqs ++ "\n"
  where
    eqs = concatMap f $ zip (take 3 input) [0..]
    f :: ((V3 Int, V3 Int), Int) -> [String]
    f ((V3 x y z, V3 vx vy vz), i) = [
        printf "x + t%d * vx == %d + t%d * %d" i x i vx,
        printf "y + t%d * vy == %d + t%d * %d" i y i vy,
        printf "z + t%d * vz == %d + t%d * %d" i z i vz
                                     ]

main24 :: IO ()
main24 = do
    input <- map parse . lines <$> readFile "res/input24"
    print $ solveA input
    putStr $ solveB input
