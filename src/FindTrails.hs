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
-- import qualified Data.Vector as V
-- import System.Random.MWC
import Debug.Trace 

import Grid
import Data.STRef
import Control.Monad.Extra (ifM)

-- | The end-points of a trail
data EPTrail = EndPoints { epSource :: Coord
                         , epSink  :: Coord 
                         } deriving (Eq, Show)

-- | The problem setup. A particular size and sources and sinks
data Grid = Grid { gSize :: (Int,Int)
                 , gTrails :: [EPTrail] 
                 }

maxCoords :: (Num t, Num t1) => (t, t1) -> (t, t1)
maxCoords (xS,yS) = (xS - 1, yS - 1)

-- | A sequence of coordinates from a source to a sink of a colour. Includes direction information.
type FTrail = [(Coord, Direction)] 

-- | Controls search, next branch, finished, or give up on the current trail
data Search a = SNext | SFinished a | SAbortTrail (EPTrail, TrColor) deriving Show

-- | Search for a single trail
type SFTrail = Search FTrail

-- | Check that for color there is a route from the source to the sink
srcReachable ::  G s -> (EPTrail, TrColor) -> ST s Bool
srcReachable gg (endpoints, color) = let distG = gDists gg ! color 
                                         size = gMaxbound gg in do
                                       dist <- minNeighbours size distG (epSource endpoints)
                                       return $! dist /= infDist
                                       
-- | Find the first location of loc, from which a sink can't be reached
srcsReachable :: G s -> [(EPTrail, TrColor)] -> ST s (Maybe (EPTrail, TrColor))
srcsReachable gg locs = fmap snd . find (not . fst) <$> 
         mapM (\loc -> do r <- srcReachable gg loc
                          return (r,loc)) locs

-- | Fill loc for the duration of body.
withOcc :: G s -> Coord -> ST s (Search t) -> ST s (Search t)
withOcc gf loc body    =  do changeLoc gf True loc  
                             res <- body
                             case res of
                               SFinished _ -> return res
                               otherwise -> do changeLoc gf False loc
                                               return res            

-- | Execute body for each location until body returns SFinished
searchFold :: (Monad m) => [a] -> (a -> m (Search b)) -> m (Search b)
searchFold [] _ = return SNext
searchFold (loc:n2Rest) body =
  do res <- body loc
     case res of
       SNext -> searchFold n2Rest body
       otherwise -> return res

{- | On success this function is called with the solved trail. 
     By returning SNext the function can request backtracking to find an alternate trail.
     By returning SFinished, we proceed to the next part (which could be the final result). -}      
type SearchCont s = OCCG s -> FTrail -> ST s (Search [FTrail])

--- Every n trace messages, give out the message
traceEvery :: Int
traceEvery = 10000         

-- | Find a route from the source to the sink. In the case of success, call the continuation onSuccess,
--   to carry on with the next color
solveIter :: G s -> Bool -> [(EPTrail,TrColor)] -> SearchCont s -> ST s (Search [FTrail])
solveIter gf noLaterReach endPoints@((trail,trailC):laterEps) onSuccess
-- (trail, trailC) is what we are currently finding a path for
-- endPoints and laterEps are only used for checking reachability
    = do msgCounter <- newSTRef 0                                        
         let startPos = epSource trail
             leadsToSink (loc,_) = reachability gf [trailC] loc
             colors = trailC : map snd laterEps
             allReachable locs = and <$> mapM (reachability gf colors . fst) locs
             loop pos direction route
               = let callLoop newPos newDir = withOcc gf pos $ loop newPos (Just newDir) $ route ++ [(pos, newDir)] in
                 if pos == sink then do
                   -- Need to make sure there are no unreachable squares next to the sink
                   singS <- singularTail =<< freeN blocked pos 
                   if null singS then onSuccess blocked route -- trail finished. continue.
                   else trace "Singular next to sink" return SNext
                 else  
                   -- do all unsolved sources have a possible route to a sink
                   snextUnreachable msgCounter (srcsReachable gf laterEps) $ do
                       freeN1 <- freeN blocked pos -- get the free neighbours
                       -- Check that from each free neighbour a sink from a later trail is reachable
                       (if noLaterReach then id
                        else snextIsolated (Just route) msgCounter (allReachable freeN1)) $ do
                           freeN3 <- (maybeAddSink pos) <$> filterM leadsToSink freeN1
                           if null freeN3 then trace "Stalled" $ return SNext
                           -- try each of the neighbours in turn in order to find a route to the sink                                  
                           else do freeN2  <- singular freeN1
                                   -- printSTDistArr dists 
                                   case freeN2 of
                                         [(newPos,newDir)] ->
                                                -- we will stall out if freeN2 does not belong to freeN3
                                                ifM (reachability gf [trailC] newPos) 
                                                    (callLoop newPos newDir)
                                                    (tracePer' msgCounter "Singular not available" newPos SNext)
                                         []  -> do freeN4 <- favorSameDirection direction <$> sortByDistance freeN3
                                                   searchFold freeN4 (\(newPos, newDir) -> callLoop newPos newDir) 
                                         others -> tracePer traceEvery msgCounter "Singulars" SNext                                        
         loop (epSource trail) Nothing []
  where maxCoord = gMaxbound gf
        blocked = gOcc gf
        mapEndPoints f = map (f . fst) endPoints
        sources = mapEndPoints epSource 
        sinks = mapEndPoints epSink
        dists = (gDists gf)!trailC
        emptyBoard :: ST s (OCCG s)
        emptyBoard = newArray ((0,0),maxCoord) False
        sink = epSink trail
        freeN = freeNeighbours maxCoord
        maybeAddSink pos locs = case neighbourD sink pos of
                                            Just dir -> (sink,dir):locs
                                            Nothing -> locs
        singular = singularNeighbours maxCoord blocked (sinks ++ sources)
        singularTail = singularNeighbours maxCoord blocked (tail sinks ++ tail sources)
        tracePer' msgCounter reason var = tracePer traceEvery msgCounter (reason ++ show var)                                     
        snextUnless reason var msgCounter cond body 
            = ifM cond body $ tracePer traceEvery msgCounter (reason ++ maybe "" show var ) SNext
        snextIsolated = snextUnless "Isolated: "
        snextUnreachable msgCounter cond body = do
           c <- cond
           case c of
              Nothing -> body
              Just ep -> do count <- readSTRef msgCounter
                            if count < 1000000 then tracePer traceEvery msgCounter ("Sink unreachable." ++ show ep) SNext
                            else return $! trace ("Sink unreachable. Aborting trail." ++ show ep) $ SAbortTrail ep
        -- Sort by reverse distance for the final color. i.e. aim for coverage
        comparison = if null laterEps then comparing $ Down . snd else comparing snd
        sortByDistance locs = map fst <$> sortBy comparison
                                 <$> mapM (\p@(loc,direction) -> (p,) <$> readArray dists loc) locs
        favorSameDirection direction locs = maybe locs 
             (\direction1 -> maybe locs (\loc -> loc:(delete loc locs)) 
                                 $ find (\(_,dir) -> dir == direction1) locs) direction 

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
solveGrid1 :: G s -> Bool -> [(EPTrail, TrColor)] -> SearchCont s -> ST s (Search [FTrail])
solveGrid1 gf noLaterReach endPoints onSuccess
   = do msgCounter <- newSTRef 0
        -- solveIter does the first trail, the continuation the rest 
        let bringTrailToFront epToFront ep = trace ("Moved to front: " ++ show epToFront)
                                                         loop (epToFront:(delete epToFront ep))
            loop eps@(_:ePoints) =
              solveIter gf noLaterReach eps (\grid route -> 
                  if null ePoints then onSuccess grid route
                  else do -- unsafeIOToST $ print route
                          res <- loop ePoints
                          -- error $ "Stop!" ++ show res
                          case res of  
                             SFinished routes -> return $ SFinished (route:routes)
                             SNext -> return SNext
                             SAbortTrail epToFront -> bringTrailToFront epToFront eps
                  )           
        result <- loop endPoints
        case result of
             SAbortTrail epToFront -> bringTrailToFront epToFront endPoints
             _ -> return result                      
                                
