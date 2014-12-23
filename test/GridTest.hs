{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GridTest where
import Grid

import Test.HUnit
import Data.Maybe

import Test.Framework

import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Control.Applicative
import Debug.Trace
             
checkDistance maxCoords occG distG sink loc = 
    do locDist <- readArray distG loc
       filled <- readArray occG loc
       if filled then 
        return $! if locDist == infDist then Nothing
                 else if loc == sink && locDist == 0 then Nothing 
                 else Just (loc, infDist, locDist)
       else do          
        minN_dist <- minNeighbours maxCoords distG loc
        let expected_dist = if minN_dist == infDist then infDist else minN_dist + 1
        return $! if expected_dist == locDist then Nothing
                  else Just (loc, expected_dist, locDist)
               
checkGrids gg  =
  do let maxCoords = gMaxbound gg
         occG  = gOcc gg
         grids = zip (elems $ gDists gg) (elems $ gSinks gg)
         checkGrid (distG, sink) = do
           assocs <- getAssocs distG 
           catMaybes <$> mapM (\(loc,_) -> 
                           checkDistance maxCoords occG distG sink loc) assocs
     mapMaybe (\error -> if null error then Nothing else Just error)
       <$> mapM checkGrid grids    
     
     -- | Test with one sink and cary out various changes recalculating each time. 
testGen2 :: ST s (G s)
testGen2 =   do gg <- initG (4,4) [(2,2)]
                let chLoc filled coord = do changeLoc gg filled coord
                                            errors <- checkGrids gg
                                            -- unsafeIOToST (print errors) 
                                            unsafeIOToST $ assertEmpty errors
                                            return ()   
                    distG = gDists gg ! 0
                mapM_ (chLoc True) [(1,1),(2,1),(3,1),(1,3),(2,3),(3,3)]
                mapM_ (chLoc True) [(1,2),(3,2)]
                mapM_ (chLoc False) [(3,2)]
                return gg
                
testDist2 =  runSTUArray $ do gg <- testGen2
                              return $ (gDists gg)!0
testDist3 = runST $ testGen2 >>= checkGrids                
                
-- | The the whole algorithm
testDist :: IO ()
testDist =  printArr testDist2                                  

test_1 :: IO ()
test_1 = do a <- return testDist2 
            seq a return ()
