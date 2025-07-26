module Grid where

import qualified Data.Map as Map

data Coordinate = Coordinate Int Int
   deriving (Show, Eq, Ord)

data GridType = Clear | Wall | Start | End
   deriving (Show, Eq)

{-- Create grid based on inputted height and width --}
makeGrid :: Int -> Int -> Map.Map Coordinate GridType
makeGrid width height = 
   Map.fromList [(Coordinate x y, Clear) | y <- [0..height-1], x <- [0..width-1]]

data Grid = Grid (Map.Map Coordinate GridType)
   deriving (Show, Eq)

{--Function to get tile information)
getTile :: Grid -> Coordinate -> Maybe GridType
getTile (Grid g) coordinate = Map.lookup coordinate g