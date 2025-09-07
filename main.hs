module Main where

import RandomGrid 
import BFS 

main :: IO ()
main = do
  g <- makeRandomGrid

  putStrLn "=== Random Grid ==="
  putStrLn "(# = wall, S = start, E = end)"
  putStrLn (renderGrid g)

  case shortestPath g of
    Nothing   -> putStrLn "No path found."
    Just path -> do
      putStrLn "\n=== Grid with Shortest Path ==="
      putStrLn "(# = wall, S = start, E = end, * = path)"
      putStrLn (renderPathOnly g path)