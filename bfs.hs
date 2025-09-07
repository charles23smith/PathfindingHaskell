module BFS where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable 
import Data.List     
import Grid

-- Extract map
gridMap :: Grid -> Map.Map Coordinate GridType
gridMap (Grid m) = m

renderGrid :: Grid -> String
renderGrid g =
  let m = gridMap g
      (xmin,xmax,ymin,ymax) = bounds g
      cellChar c =
        case Map.lookup c m of
          Just Wall  -> '#'
          Just Start -> 'S'
          Just End   -> 'E'
          Just Clear -> ' '
          Nothing    -> ' '
      row y = [ cellChar (Coordinate x y) | x <- [xmin..xmax] ]
      rows  = [ row y | y <- [ymin..ymax] ]
  in unlines rows

-- Start/End
findStartEnd :: Grid -> Maybe (Coordinate, Coordinate)
findStartEnd g =
  let m = gridMap g
      starts = [ c | (c, Start) <- Map.toList m ]
      ends   = [ c | (c, End)   <- Map.toList m ]
  in case (starts, ends) of
       (s:_, e:_) -> Just (s,e)
       _          -> Nothing

-- Bounds inferred from keys
bounds :: Grid -> (Int,Int,Int,Int)  -- (xmin,xmax,ymin,ymax)
bounds g =
  let ks = Map.keys (gridMap g)
      xs = [x | Coordinate x _ <- ks]
      ys = [y | Coordinate _ y <- ks]
  in (minimum xs, maximum xs, minimum ys, maximum ys)

-- 4-neighbors, passable
neighbors :: Grid -> Coordinate -> [Coordinate]
neighbors g (Coordinate x y) =
  let (xmin,xmax,ymin,ymax) = bounds g
      cand = [ Coordinate (x+1) y
             , Coordinate (x-1) y
             , Coordinate x (y+1)
             , Coordinate x (y-1)
             ]
      inBounds (Coordinate i j) = i>=xmin && i<=xmax && j>=ymin && j<=ymax
      passable c =
        case Map.lookup c (gridMap g) of
          Just Wall -> False
          Just _    -> True
          Nothing   -> False
  in filter (\c -> inBounds c && passable c) cand

-- BFS that records:
--   parent map (for path), distance map, and discovery order
bfsWithOrder
  :: Grid -> Coordinate
  -> (Map.Map Coordinate Coordinate, Map.Map Coordinate Int, [Coordinate])
bfsWithOrder g s0 =
  let go seen parent dist q order =
        case q of
          Seq.Empty      -> (parent, dist, order)
          (v Seq.:<| qs) ->
            let ns  = [ n | n <- neighbors g v, not (Set.member n seen) ]
                -- discover in this order
                seen'   = foldr Set.insert seen ns
                parent' = foldr (\n m -> Map.insert n v m) parent ns
                dist'   = foldr (\n m -> Map.insert n (1 + dist Map.! v) m) dist ns
                q'      = qs <> Seq.fromList ns
                order'  = order ++ ns
            in go seen' parent' dist' q' order'
  in go (Set.singleton s0) Map.empty (Map.singleton s0 0) (Seq.singleton s0) [s0]

-- Reconstruct end→start using parent map
reconstruct :: Coordinate -> Coordinate -> Map.Map Coordinate Coordinate -> [Coordinate]
reconstruct s e parent =
  reverse (go e)
  where
    go cur
      | cur == s  = [s]
      | otherwise = case Map.lookup cur parent of
                      Nothing -> []   -- unreachable
                      Just p  -> cur : go p

shortestPath :: Grid -> Maybe [Coordinate]
shortestPath g = fst (shortestPathAndOrder g)

bfsOrder :: Grid -> [Coordinate]
bfsOrder g = snd (shortestPathAndOrder g)

shortestPathAndOrder :: Grid -> (Maybe [Coordinate], [Coordinate])
shortestPathAndOrder g =
  case findStartEnd g of
    Nothing      -> (Nothing, [])
    Just (s, e)  ->
      let (parent, _dist, order) = bfsWithOrder g s
          path = reconstruct s e parent
      in (if null path then Nothing else Just path, order)

-- Optional: distances reachable from Start
reachableFromStart :: Grid -> Map.Map Coordinate Int
reachableFromStart g =
  case findStartEnd g of
    Nothing     -> Map.empty
    Just (s, _) ->
      let (_, d, _) = bfsWithOrder g s
      in d

renderWithExploredAndPath :: Grid -> [Coordinate] -> [Coordinate] -> String
renderWithExploredAndPath g explored path =
  let m = gridMap g
      (xmin,xmax,ymin,ymax) = bounds g
      exploredS = Set.fromList explored
      pathS     = Set.fromList path
      cellChar c =
        case Map.lookup c m of
          Just Wall  -> '#'
          Just Start -> 'S'
          Just End   -> 'E'
          Just Clear ->
            if Set.member c pathS then '*'
            else if Set.member c exploredS then '.'
            else ' '
          Nothing    -> ' '  -- shouldn't happen with well-formed grid
      row y = [ cellChar (Coordinate x y) | x <- [xmin..xmax] ]
      rows  = [ row y | y <- [ymin..ymax] ]
  in unlines (map id rows)

renderPathOnly :: Grid -> [Coordinate] -> String
renderPathOnly g path = renderWithExploredAndPath g [] path
