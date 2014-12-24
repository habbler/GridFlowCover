{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FindFlowCover where

import Control.Monad
import Control.Monad.ST
import Control.Applicative
import qualified Data.Array as BA
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe
import Data.List
import qualified Data.Vector as V
-- import System.Random.MWC
import Debug.Trace 

import Grid
import Data.STRef
import Control.Monad.Extra (ifM)

-- | The end-points of a trail
data EPTrail = EndPoints { epSource :: Coord
                         , epSink  :: Coord 
                         } deriving Show

-- | The problem setup. A particular size and sources and sinks
data Grid = Grid { gSize :: (Int,Int)
                 , gTrails :: [EPTrail] 
                 }

maxCoords :: (Num t, Num t1) => (t, t1) -> (t, t1)
maxCoords (xS,yS) = (xS - 1, yS - 1)

type FTrail = [(Coord, Direction)] 

data Search a = SNext | SFinished a deriving Show

type SFTrail = Search FTrail

-- | Check that for color there is a route from the source to the sink
srcReachable ::  G s -> (EPTrail, Int) -> ST s Bool
srcReachable gg (endpoints, color) = let distG = gDists gg ! color in do
                                       dist <- readArray distG (epSource endpoints)
                                       return $! dist /= infDist
                                       
-- | Check at all locations are reachable from at least one sink
srcsReachable :: G s -> [(EPTrail, Int)] -> ST s Bool
srcsReachable gg locs = and <$> mapM (srcReachable gg) locs

-- | Fill loc for the duration of body.
withOcc :: G s -> Coord -> ST s (Search t) -> ST s (Search t)
withOcc gf loc body    =  do changeLoc gf True loc  
                             res <- body
                             case res of
                               SFinished _ -> return res
                               SNext -> do changeLoc gf False loc
                                           return res

-- | Execute body for each location until body returns SFinished
searchFold :: (Monad m) => [a] -> (a -> m (Search b)) -> m (Search b)
searchFold [] _ = return SNext
searchFold (loc:n2Rest) body =
  do res <- body loc
     case res of
       SFinished _ -> return res
       otherwise -> searchFold n2Rest body

-- | Find a route from the source to the sink. In the case of success, call the continuation cont,
-- | to carry on with the next color
solveIter :: G s -> [(EPTrail,TrColor)] -> (OCCG s -> FTrail -> ST s (Search a)) -> ST s (Search a)
solveIter gf ((trail,trailC):laterEps) cont
       = do msgCounter <- newSTRef 0                                        
            let leadsToSink (loc,_) = reachability gf [trailC] loc
                colors = (trace $ show trail) trailC : map snd laterEps
                allReachable locs = and <$> mapM (reachability gf colors . fst) locs
                loop (pos, route)
                  = if pos == sink then cont board route -- trail finished. continue.
                    else withOcc gf pos $ -- trace (show res) $ return res
                  -- do all unsolved sources have a possible route to a sink
                      snextUnless (srcsReachable gf laterEps)  "sink unreachable" $ do
                            freeN1 <- freeN board pos -- get the free neighbours
                            -- Check that from each free neighbour a sink is reachable
                            snextIsolated msgCounter (allReachable freeN1) route $ do
                                  freeN3 <- (maybeAddSink pos) <$> filterM leadsToSink freeN1
                                  if null freeN3 then trace "Stalled" $ return SNext
                                  -- try each of the neighbours in turn in order to find a route to the sink
                                  else do freeN2  <- singularToFront freeN3
                                          searchFold freeN2 (\(newPos, direction) -> 
                                                   loop (newPos, route ++ [(pos, direction)]) )
            out <- loop (epSource trail,  [])
            unsafeIOToST (print "End of loop.")
            return out
  where maxCoord = gMaxbound gf
        board = gOcc gf 
        emptyBoard :: ST s (OCCG s)
        emptyBoard = newArray ((0,0),maxCoord) False
        sink = epSink trail
        freeN = freeNeighbours maxCoord
        maybeAddSink pos locs = case neighbourD sink pos of
                                            Just dir -> (sink,dir):locs
                                            Nothing -> locs
        singularToFront locs =   do singNb <- singularNeighbours maxCoord board locs
                                    return $! singNb ++ (locs \\ singNb)
                                    
        snextUnless cond reason body = ifM cond body $ return $! trace reason SNext
        snextIsolated msgCounter cond var body 
             = ifM cond body $ tracePer 1000 msgCounter ("Isolated: " ++ show var) SNext

tracePer :: Integral a => a -> STRef s a -> String -> b -> ST s b
tracePer every msgCounter msg value = 
                  do modifySTRef msgCounter (+1) 
                     count <- readSTRef msgCounter 
                     if mod count every == 0 then return $! trace msg value
                                             else return value       


-- | For each color find a route, search by backtracking (SNext triggers backtracking)
solveGrid1 :: G s -> [(EPTrail, TrColor)] -> ST s (Search [FTrail])
solveGrid1 gf eps@(_:ePoints) -- solveIter does the first trail, the continuation the rest
   =   do msgCounter <- newSTRef 0 
          solveIter gf eps (\grid route -> 
            if null ePoints then do 
                 elems <- getElems grid
                 if and elems then return $ SFinished [route] 
                 else tracePer 1000 msgCounter "Missed" SNext 
            else do res <- solveGrid1 gf ePoints
                    return $! case res of  
                                 (SFinished routes) -> SFinished (route:routes)
                                 SNext -> SNext )
                        
-- | Solve the Grid
testIter :: Grid -> [FTrail]
testIter (Grid size endpoints) = runST $  do gf <- createInitialGrid
                                          -- grid <- initBoolG maxBound False
                                             result <- solveGrid1 gf $ zip endpoints [0,1..]
                                             case result of 
                                               SFinished trail -> return trail
                                               SNext -> error "No solution exists"
  where maxBound = maxCoords size
        createInitialGrid = initG maxBound $ map epSink endpoints

-- | Solve the grid and return solutions as coordinates                           
traceTrail :: Grid -> [[Coord]]                          
traceTrail grid = map (\trail -> scanl updateCoord (fst $ head trail) $ map snd trail)
                      $ testIter grid 

trailGrid :: Grid -> [[Coord]]
trailGrid = traceTrail

 
