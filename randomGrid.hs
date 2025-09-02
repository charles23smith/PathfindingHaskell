module RandomGrid where

import System.Random
import qualified Data.Map as Map
import Grid


{--make random 7x7 grid with one start, one end, and a chance at either of the other tiles--}
makeRandomGrid :: IO Grid
makeRandomGrid = do
   sx <- randomRIO (0,6)
   ex <- randomRIO (0,6)

   --random rolls for each tile
   topRowRolls <- mapM (\_ -> randomRIO (1,7 :: Int)) [0..6]   --1/7 chance of walls
   bottomRowRolls <- mapM (\_ -> randomRIO (1,7 :: Int)) [0..6]   --1/7 chance of walls
   let middleRowCoordinates = [Coordinate x y | y <- [1..5], x <- [0..6]]
   middleRowRolls <- mapM (\_ -> randomRIO (1,7 :: Int)) middleRowCoordinates  --2/7 chance of walls

   let allCoords = [Coordinate x y | y <- [0..6], x <- [0..6]]
       base      = Map.fromList [(c, Clear) | c <- allCoords]

       topWalls = [Coordinate x 0 | (x,r) <- zip [0..6] topRowRolls, x/= sx, r==1]
       bottomWalls = [Coordinate x 6 | (x,r) <- zip [0..6] bottomRowRolls, x/= ex, r==1]
       middleWalls= [c | (c,r) <- zip middleRowCoordinates middleRowRolls, r<=2]

       withWalls = foldr (\c m -> Map.insert c Wall m) base (topWalls ++ bottomWalls ++ middleWalls)
       withStart = Map.insert (Coordinate sx 0) Start withWalls
       withEnd = Map.insert (Coordinate ex 6) End withStart

   pure (Grid withEnd)