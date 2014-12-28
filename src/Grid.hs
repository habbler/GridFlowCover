{-# LANGUAGE ScopedTypeVariables #-}
module Grid where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Extra
import qualified Data.Foldable as F
import qualified Data.Array as BA
import Data.Array.ST
import Data.Array.Unboxed
import Data.Word
import Data.Maybe
import Data.List
import Debug.Trace

import Text.Printf



type Coord = (Int, Int)
type BoardSize = (Int, Int)
type MaxCoords = (Int, Int)
type Direction = Int
type TrColor = Int
type OCCG s = STUArray s Coord Bool
type DistG s = STUArray s Coord Distance
type Distances s = Array TrColor (DistG s)
type Sinks = Array TrColor Coord

type Distance = Word8
infDist :: Distance
infDist = 255

data G s = GF { gMaxbound :: Coord
              , gOcc :: OCCG s -- The array of squares that are blocked
              , gDists :: Distances s -- one distance array for each sink.
              , gSinks :: Sinks  -- the locations of the sinks  
              } 
              
-- | Find the coord in direction d from give coord
updateCoord :: Coord -> Direction -> Coord
updateCoord (x,y) direction = case direction of 
                                    0 -> (x,y+1) -- N
                                    1 -> (x+1,y) -- E
                                    2 -> (x,y-1) -- S  
                                    3 -> (x-1,y) -- W

-- | Find the direction from the first coord to the second - inverse of updateCoord
neighbourD :: Coord -> Coord -> Maybe Direction
neighbourD (xl, yl) (xn, yn)
  | xl == xn =
    case yl - yn of
        - 1 -> Just 2
        1 -> Just 0
        otherwise -> Nothing
  | yl == yn =
    case xl - xn of
        - 1 -> Just 3
        1 -> Just 1
        otherwise -> Nothing
  | otherwise = Nothing

testNd = neighbourD (1,1) (2,1)                            

isNeighbourOf :: Coord -> Coord -> Bool
isNeighbourOf c1 c2 = isJust (neighbourD c1 c2)

anyNeighbourOf :: [Coord] -> Coord -> Bool
anyNeighbourOf cs coord = any (isNeighbourOf coord) cs 
                                    
{-# inline neighbours #-}
-- | Apply function f to the neighbours of r0 taking into account the boundaries                                    
neighbours :: (Monad m) => r0 -> (Coord -> Direction -> r0 -> m r0) -> MaxCoords -> Coord -> m r0
neighbours r0 f (bX,bY) (x,y) 
  = do r1 <- ifm (y < bY) (x,y+1) 0  r0
       r2 <- ifm (x < bX) (x+1,y) 1  r1
       r3 <- ifm (y > 0)   (x,y-1) 2  r2
       ifm       (x > 0)   (x-1,y) 3  r3
  where ifm c l d r = if c then f l d r else return r
  

                                 
-- | Return the neighbours where occG is false
falseNeighbours ::  MaxCoords -> OCCG s -> Coord -> ST s [Coord]
falseNeighbours maxCoords occG (x,y) = neighbours [] ifm maxCoords (x,y) 
  where ifm l d r = do occ <- readArray occG l
                       if occ then return r else return $! l:r

-- | Return the neighbours where occG is false, plus the direction to them                        
freeNeighbours ::  MaxCoords -> OCCG s -> Coord -> ST s [(Coord, Direction)]
freeNeighbours maxCoords occG = neighbours [] ifm maxCoords 
  where ifm l d r = do occ <- readArray occG l
                       if occ then return r else return $! (l,d):r

-- | Count the number of free neighbours (occG is false)                         
countFreeNeighbours ::  MaxCoords -> OCCG s -> Coord -> ST s Int
countFreeNeighbours maxCoords occG = neighbours 0 ifm maxCoords 
  where ifm l d r = do occ <- readArray occG l
                       if occ then return r else return $! r + 1 

                     
-- | Find the lowest value from distG among the neighbours
minNeighbours ::  MaxCoords -> DistG s -> Coord -> ST s Distance
minNeighbours maxCoords distG = neighbours infDist ifm maxCoords
  where ifm l d r = do val <- readArray distG l
                       return $! if val < r then val else r
                       
-- | Initialise a bool array                       
initBoolG :: MaxCoords -> Bool -> ST s (OCCG s)   
initBoolG maxCoords = newArray ((0,0), maxCoords)

-- | Initialise a Word8 array 
initDistG :: MaxCoords -> Word8 -> ST s (DistG s)
initDistG maxCoords = newArray ((0,0), maxCoords)

-- | call function f repeatedly on its own output list, until that is empty           
processLayersM_ :: Monad m => ([t] -> m [t]) -> [t] -> m ()
processLayersM_ f [] = return()       
processLayersM_ f l = f l >>= processLayersM_ f

copyArray :: (Ix i, MArray a e m) => a i e -> m (a i e)
copyArray = mapArray id

-- | Update the distance array occG based on changed location lastChanged                     
updateDists :: MaxCoords -> OCCG s -> DistG s -> Coord -> ST s ()
updateDists maxCoords occG distG lastChanged                          
  = do doneG <- copyArray occG
       let notInf loc  = do dist <- readArray distG loc 
                            return $! dist /= infDist
           -- set a location to infinity and returns the neigbours that need changing
           -- these come in two types, those which we set to infinity, and the others.                 
           fSetInf (accumMins, accumInf) loc  =
             do locDist  <- readArray distG loc
                -- already processed, move on.
                if locDist == infDist then return (accumMins, accumInf)
                else do minN_dist  <- minNeighbours maxCoords distG loc
                        writeArray distG loc infDist
                        -- when we have neighbours that are smaller than us, we can update this square to lower?
                        -- how do we know the neighbours are already correct?
                        -- we will however finish infinities before returing loc...
                        if minN_dist <= locDist then
                          return (loc:accumMins, accumInf)
                        -- otherwise if it is larger than us, it might be deriving it distance from us,
                        -- which is now invalid. We set all non infinity neighbours onto the processing queue  
                        else do neighbours <- falseNeighbours maxCoords occG loc
                                n1 <- filterM notInf neighbours
                                return (accumMins, accumInf ++ n1)

           fCalcMin = fCalcMinW maxCoords distG doneG
           -- recursively calculate distances. First the neighbours, then the neighbours of those, etc.
           fDistances [] [] = return ()
           fDistances calcMins [] = processLayersM_ fCalcMin calcMins
           fDistances calcMins setInf =
              do newMins1 <- fCalcMin calcMins
                 (newMins2, newInfs) <- foldM fSetInf ([],[]) setInf 
                 fDistances (newMins1 ++ newMins2) newInfs
                             
       filled <- readArray occG lastChanged
       if filled then 
        do -- dist <- readArray distG lastChanged
           -- when (dist == 0) $ error "dist == 0"
           writeArray distG lastChanged infDist
           neighbours <- falseNeighbours maxCoords occG lastChanged
           -- distances can only get worse when we add a blockage / remove a possible route
           fDistances [] neighbours -- set the neighbours to infinity and spread out from there.
        -- distances can only get better when we remove a blockage / add a new route   
        else fDistances [lastChanged] []
        
-- | Given a list of locations coords, updates all of them to +1 of the lowest of its neighbours
-- | and returns a new set of locations that need updating
fCalcMinW :: (Int, Int) -> DistG s -> OCCG s -> [Coord] -> ST s [Coord]
fCalcMinW maxCoords distG doneG coords  = foldM fCalcMin [] coords 
  where fCalcMin accumsMins loc =
             do locDist <- readArray distG loc
                minN_dist <- minNeighbours maxCoords distG loc
                writeArray doneG loc True
                -- need to ensure minN_dist is nevery infinity !!!
                let newDist = minN_dist + 1
                if locDist /= minN_dist then
                  do writeArray distG loc newDist
                     neighbours <- falseNeighbours maxCoords doneG loc
                     return (accumsMins ++ neighbours)
                else return accumsMins
                
                
                

       
-- | Returns a distance array give a array of blocked positions and a destination sinkLoc                   
initDist  :: MaxCoords -> OCCG s -> Coord -> ST s (DistG s)
initDist maxCoords occG sinkLoc 
  = do distG <- initDistG maxCoords infDist
       writeArray distG sinkLoc 0
       doneG <- copyArray occG
       processLayersM_ (fCalcMinW  maxCoords distG doneG)  [sinkLoc] 
       return distG
       
-- | Given a list of sinks return the structure of distance arrays. All other squares are empty
initG :: MaxCoords -> [Coord] -> ST s (G s)       
initG maxbound sinks 
    =  do occG <- initBoolG maxbound False
          -- We need to add all sinks to occG before we can start
          -- calculating any distances!!!
          mapM_ (\loc -> writeArray occG loc True) sinks 
          distG <- mapM (initDist maxbound occG) sinks
          let numSinks = length sinks
              createColorArr :: [e] -> Array Int e
              createColorArr = listArray (0,numSinks-1)
              dists   = createColorArr distG
              sinkArr = createColorArr sinks                           
          return $! GF maxbound occG dists sinkArr
                           
-- | When location is set to val, then update all distance arrays                          
changeLoc :: G s -> Bool -> Coord -> ST s ()
changeLoc gg val loc 
  = do
       writeArray occG loc val 
       mapM_ (\distG -> updateDists maxbound occG distG loc) $ elems (gDists gg) 
  where maxbound = gMaxbound gg
        occG = gOcc gg

-- | Check that each source is reachable from this location for the colors specified
reachability ::  G s -> [TrColor] -> Coord -> ST s Bool
reachability gg colors loc = anyM check colors 
  where distG = gDists gg 
        check c =  (/= infDist) <$> readArray (distG!c) loc 
                           
-- | Locations with 0 or 1 ways in or out of them (note that the current trail head
-- 
singularNeighbours :: MaxCoords -> OCCG s -> [Coord] -> [(Coord, t)] -> ST s [(Coord, t)]
singularNeighbours maxCoords occG endPoints neighb 
  = liftM catMaybes $ mapM (\nb@(nbC,_) ->  do nCnt <- countFreeNeighbours maxCoords occG nbC
                                               return $!if nCnt > 1 then Nothing
                                                        else if nCnt == 0 then Just nb
                                                        else if anyNeighbourOf endPoints nbC then Nothing
                                                        else Just nb 
                                            ) neighb

-- | Print out an array of numbers                     
printArr :: (IArray a e, PrintfArg e) => a (Int, Int) e -> IO ()
printArr = printArray (printf "%3d") 5

-- | print out an array of values using function pf for each value
printArray :: (IArray a e) => (e -> String) -> Int -> a (Int, Int) e -> IO ()                
printArray pf colWidth array 
    = putStr $   "   " ++ concatMap (\ c -> padw [c]) (take (x2 - x1 + 1) colLabels) ++
  "\n" ++ concatMap rowOut [y1 .. y2]
                        where colHeader c = printf "%3c" c
                              padw = pad colWidth
                              ((x1,y1),(x2,y2)) = bounds array 
                              colLabels = "ABCDEFGHIJKLMNOPQRSTUVWXZY"
                              rowOut y = printf "%3d" y ++ concatMap output [x1..x2] ++ "\n"
                               where output x = padw $ pf (array!(x,y))
                                      
-- | Right justify in the field of length n                                     
pad :: Int -> String -> String
pad n str = let l = length str in
              if l < n then replicate (n - l) ' ' ++ str 
              else str                                     

