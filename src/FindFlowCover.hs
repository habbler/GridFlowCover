module FindFlowCover where

import Control.Monad.ST
import Data.Array.MArray (getElems)

import Grid (initG, updateCoord, Coord, Direction)
import FindTrails


-- | Solve the Grid used depth wise back tracking search
searchForTrails :: Grid -> [FTrail]
searchForTrails (Grid size endpoints) = runST $ do
        -- Note that we can't set the sources to blocked in the current design,
        -- as we need distances to be calculated for the sources, as reachability
        -- is defined as the distance not being infinity.
        -- This means that we need to exclude sources as being an empty square during the search...  
    gf <- initG (maxCoords size) (map epSink endpoints)
    result <- solveGrid1 gf (zip endpoints [0,1..])
                -- Success continuation 
                (\grid route -> return $ SFinished [route]) 
    case result of 
      SFinished trail -> return trail
      SNext -> error "No solution exists"

-- | Solve the Grid used depth wise back tracking search
searchForCover :: Grid -> [FTrail]
searchForCover (Grid size endpoints) = runST $ do
    gf <- initG maxBound (map epSink endpoints)
    -- grid <- initBoolG maxBound False
    result <- solveGrid1 gf (zip endpoints [0,1..])
                (\grid route -> do
                 -- Here we are at the bottom of the recursion. We only have the route
                 -- of the last color.
                 -- Check whether we have a complete cover. 
                 elems <- getElems grid
                 let unCovCount = length $ filter not elems
                 -- I am guessing it is the case that if there is a solution with
                 -- exactly one square uncovered, then there is no solution with 
                 -- all squares covered.
                 if unCovCount < 2  then return $ SFinished [route]
                 -- In this case we would like to show all the routes to see how
                 -- close we got, but not too often. How to do that?
                 -- Would need to accumulate the routes. 
                 else return SNext) 
                
    case result of 
      SFinished trail -> return trail
      SNext -> error "No solution exists"
  where maxBound = maxCoords size

-- | Solve the grid and return solutions as coordinates                           
--traceTrail :: Grid -> [[Coord]]                          
traceTrail :: [FTrail] -> [[Coord]]
traceTrail = map (\trail -> scanl updateCoord (fst $ head trail) $ map snd trail)

trailFind :: Grid -> [[Coord]]
trailFind = traceTrail . searchForTrails
                       
trailCover :: Grid -> [[Coord]]
trailCover = traceTrail . searchForCover

 
