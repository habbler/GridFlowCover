-- | Uses the diagrams package to draw the resulting trails on a grid
-- Output the results as an SVG file.
module GFC_Diagrams( main, genOutput, genOutputF) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
-- import Text.Blaze.Svg.Renderer.Utf8

import FindTrails (Grid(..), EPTrail(EndPoints))
import FindFlowCover 
import Grid (Coord)

-- plotCenter :: (Int,Int) -> (Int,Int) -> (Double,Double)

-- | Find the center of the square at location (x,y). Used to ensure that the trail
-- goes through the center for each square.
plotCenter :: (Fractional t1, Fractional t2, Integral a, Integral a1) =>
                t -> (a, a1) -> (t1, t2)
plotCenter _size (x,y) = (fscale x , fscale y )
  where fscale w = fromIntegral w + 0.5

-- testTrail =  fromVertices $ map (p2 . (plotCenter (2,2))) [(0,0),(1,0),(1,1),(0,1)]
-- | Draws the trail on the grid
testTrailD :: (Integral a) => t -> [(a, a)] -> QDiagram SVG V2 Float Any
testTrailD  size' trail =  lw 1.5 $ fromVertices $ map (p2 . scaleGrid) trail -- 
                      where scaleGrid = plotCenter size' -- (gSize grid)

-- | Puts a border around the grid                       
borderD :: (Integral t) => (t,t) -> QDiagram SVG V2 Float Any
borderD size' = fromVertices $ map p2 [(0,0),(xB,0),(xB,yB),(0,yB),(0,0)]
                where (xS,yS) = size' -- gSize grid
                      xB = fromIntegral xS
                      yB = fromIntegral yS

-- | Draws all the trails on the grid                      
plotTrails :: (Integral a) => (a, a) -> [[(a, a)]] -> QDiagram SVG V2 Float Any
plotTrails size' trails = (trailD `atop` borderD size') # centerXY # pad 1.1
  where trailD = mconcat $ map (testTrailD size') trails

-- | Calls method and draws the resulting trails on the grid
testOutputD :: (Grid -> [[Coord]]) -> Grid -> QDiagram SVG V2 Float Any
testOutputD method grid = plotTrails (gSize grid) (method grid)
        
genOutput :: Grid -> IO ()
genOutput grid = defaultMain (testOutputD trailCover grid)

genOutputF :: Grid -> IO ()
genOutputF grid = defaultMain (testOutputD trailFind grid)

--testGrid9 = Grid (8,8) [EndPoints (2,0) (3,0), EndPoints (4,0) (5,0), EndPoints (1,0) (6,0),EndPoints (0,0) (7,0) ]
testGrid9a = Grid (8,8) [EndPoints (1,0) (6,0), EndPoints (2,1) (3,1), EndPoints (0,0) (7,0), EndPoints (4,1) (5,1) ]
testGrid9b = Grid (8,8) [EndPoints (1,0) (6,0), EndPoints (2,1) (3,1), EndPoints (0,7) (7,7), EndPoints (4,1) (5,1) ]

testGrid12a = Grid (8,8) [EndPoints (2,0) (3,2), EndPoints (4,4) (5,5), EndPoints (1,3) (6,7),EndPoints (0,0) (7,0) ]
testGrid13 = Grid (8,8) $ reverse [EndPoints (0,2) (7,3), EndPoints (5,0) (5,6)]
testGrid14 = Grid (9,9) $ take 7
 [EndPoints (3,5) (6,5), EndPoints (1,2) (5,1), EndPoints (7,1) (5,3),
  EndPoints (2,2) (6,4), EndPoints (3,2) (6,6),  EndPoints (5,4) (7,0),
  EndPoints (5,0) (8,0)
 ]
testGrid14a = Grid (9,9) $ take 2
 [EndPoints (5,0) (8,0), EndPoints (5,4) (7,0), EndPoints (3,5) (6,5), EndPoints (1,2) (5,1), EndPoints (7,1) (5,3),
  EndPoints (2,2) (6,4), EndPoints (3,2) (6,6)    
 ]
testGrid14b = Grid (9,9) $ take 2
 [EndPoints (5,4) (7,0), EndPoints (5,0) (8,0), EndPoints (3,5) (6,5), EndPoints (1,2) (5,1), EndPoints (7,1) (5,3),
  EndPoints (2,2) (6,4), EndPoints (3,2) (6,6)    
 ]

-- Trivial
testGrid14c = Grid (9,9) [EndPoints (8,1) (8,0), EndPoints (5,4) (7,0), EndPoints (3,5) (6,5)]
-- Fails
testGrid14d = Grid (9,9) [EndPoints (5,4) (7,0), EndPoints (3,5) (6,5), EndPoints (8,1) (8,0)]

-- out1 = renderSvg $ renderDia SVG (SVGOptions (Dims 200 200) Nothing) (testOutputD testGrid1)        

main :: IO () 
main = genOutput testGrid9a
main1 = defaultMain (plotTrails (9,9) [map fst 
          [((5,4),2),((5,3),2),((5,2),2),((5,1),1),((6,1),1),((7,1),2)]
            ])
-- :main -o test2.svg -w 400
 
