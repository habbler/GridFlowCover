{-# LANGUAGE ScopedTypeVariables,  FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FindTrails where
{--
Do those points close together last?
Handle different types of dead-end differently?
Those trails that are next to empty squares can be recomputed! 
Definitely makes sense then to do cover in a second phase where we recompute trails next to
empty squares.
If a trail takes a long time to find we can try searching for that trail first.
Try finding trails in different orders.
Is there some way we could handle the trails somewhat independently?
Have a function that takes a trail as input, and finds the next trail.
The easiest would be for the first half, we include the remaining choices as well.
Calculating the next possible trail is then trivial. For speed we will probably want to use
a sequence so we can access the split point quickly.
Do different variants in parallel.
Probabilistic search? Do all trails in parallel. Give priority to trails that didn't finish?
How to time out?
Treat each trail as a gene.
Does the final trail try and fill all space?
If there is some space, chop out a section of nearby trail and recompute that.
--}

import Control.Monad
import Control.Monad.ST
import Control.Applicative
import qualified Data.Array as BA
import Data.Array.ST
import Data.Array.Unboxed
import Data.Maybe
import Data.List
import Data.Ord
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

-- | On success this function is called with the solved trail. 
-- | By returning SNext the function can request backtracking to find an alternate trail.
-- | By returning SFinished, we proceed to the next part (which could be the final result).       
type SearchCont s = OCCG s -> FTrail -> ST s (Search [FTrail])        

-- | Find a route from the source to the sink. In the case of success, call the continuation onSuccess,
-- | to carry on with the next color
solveIter :: G s -> [(EPTrail,TrColor)] -> SearchCont s -> ST s (Search [FTrail])
solveIter gf endPoints@((trail,trailC):laterEps) onSuccess
-- (trail, trailC) is what we are currently finding a path for
-- endPoints and laterEps are only used for checking reachability
    = do msgCounter <- newSTRef 0                                        
         let leadsToSink (loc,_) = reachability gf [trailC] loc
             colors = trailC : map snd laterEps
             allReachable locs = and <$> mapM (reachability gf colors . fst) locs
             loop (pos, route)
               = if pos == sink then onSuccess blocked route -- trail finished. continue.
                 else withOcc gf pos $ -- trace (show res) $ return res
               -- do all unsolved sources have a possible route to a sink
                   snextUnreachable msgCounter (srcsReachable gf laterEps) $ do
                       freeN1 <- freeN blocked pos -- get the free neighbours
                       -- Check that from each free neighbour a sink is reachable
                       snextIsolated (Just route) msgCounter (allReachable freeN1) $ do
                           freeN3 <- (maybeAddSink pos) <$> filterM leadsToSink freeN1
                           if null freeN3 then trace "Stalled" $ return SNext
                           -- try each of the neighbours in turn in order to find a route to the sink                                  
                           else do freeN2  <- singular freeN3
                                   case freeN2 of
                                         [(newPos1,direction1)] -> 
                                                loop (newPos1, route ++ [(pos, direction1)])
                                         []  -> do freeN4 <- sortByDistance freeN3
                                                   searchFold freeN4 (\(newPos, direction) -> 
                                                     loop (newPos, route ++ [(pos, direction)])) 
                                         others -> tracePer 1000 msgCounter "Singulars" SNext
         loop (epSource trail,  [])
  where maxCoord = gMaxbound gf
        blocked = gOcc gf
        sources = map (epSource . fst) endPoints 
        sinks = gSinks gf
        dists = (gDists gf)!trailC
        emptyBoard :: ST s (OCCG s)
        emptyBoard = newArray ((0,0),maxCoord) False
        sink = epSink trail
        freeN = freeNeighbours maxCoord
        maybeAddSink pos locs = case neighbourD sink pos of
                                            Just dir -> (sink,dir):locs
                                            Nothing -> locs
        singular = singularNeighbours maxCoord blocked sources sinks                                    
        snextUnless reason var msgCounter cond body 
            = ifM cond body $ tracePer 1000 msgCounter (reason ++ maybe "" show var ) SNext
        snextIsolated = snextUnless "Isolated: "
        snextUnreachable = snextUnless "Sink unreachable." (Nothing :: Maybe ())
        -- Sort by reverse distance for the final color. i.e. aim for coverage
        comparison = if null laterEps then comparing $ Down . snd else comparing snd
        comparison1 = comparing $ snd   
        sortByDistance locs = map fst <$> sortBy comparison
                                 <$> mapM (\p@(loc,direction) -> (p,) <$> readArray dists loc) locs      

-- Note that the current square has already been filled, and singular checks for empty neighbours.
-- The sink is also filled. Singular returns in the case of zero or one free neighbour.
-- If zero, then we must take it, this can pay off if a sink is a neighbour.
-- If there is one empty and no sink we must take it.
-- If thee is one empty, and the sink, then we can ignore it.
-- Sound like the sink should add one on...           

toFront :: Eq a => [a] -> [a] -> [a]
toFront a b = a ++ (b \\ a)                       

tracePer :: Integral a => a -> STRef s a -> String -> b -> ST s b
tracePer every msgCounter msg value = 
                  do modifySTRef msgCounter (+1) 
                     count <- readSTRef msgCounter 
                     if mod count every == 0 then return $! trace msg value
                                             else return value       


-- | For each color find a route, search by backtracking (SNext triggers backtracking)
solveGrid1 :: G s -> [(EPTrail, TrColor)] -> SearchCont s -> ST s (Search [FTrail])
solveGrid1 gf endPoints onSuccess
   =   do msgCounter <- newSTRef 0
          -- solveIter does the first trail, the continuation the rest 
          let loop eps@(_:ePoints) 
               = solveIter gf eps (\grid route -> 
                    if null ePoints then do onSuccess grid route
                    else do res <- loop ePoints
                            return $! case res of  
                                         SFinished routes -> SFinished (route:routes)
                                         SNext -> SNext )
          loop endPoints                           
                                
