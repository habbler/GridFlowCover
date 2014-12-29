module GFC_Diagrams( main, genOutput, genOutputF) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Text.Blaze.Svg.Renderer.Utf8

import FindTrails (Grid(..), EPTrail(EndPoints))
import FindFlowCover 
import Grid (Coord)

-- plotCenter :: (Int,Int) -> (Int,Int) -> (Double,Double)

plotCenter :: (Fractional t1, Fractional t2, Integral a, Integral a1) =>
                t -> (a, a1) -> (t1, t2)
plotCenter size (x,y) = (fscale x , fscale y )
  where fscale w = fromIntegral w + 0.5

-- testTrail =  fromVertices $ map (p2 . (plotCenter (2,2))) [(0,0),(1,0),(1,1),(0,1)]

testTrailD :: (Integral a) => t -> [(a, a)] -> QDiagram SVG R2 Any
testTrailD  size trail =  lw 0.2 $ fromVertices $ map (p2 . scaleGrid) trail -- 
                      where scaleGrid = plotCenter size -- (gSize grid)
                       
borderD :: (Integral t) => (t,t) -> QDiagram SVG R2 Any
borderD size = fromVertices $ map p2 [(0,0),(xB,0),(xB,yB),(0,yB),(0,0)]
                where (xS,yS) = size -- gSize grid
                      xB = fromIntegral xS
                      yB = fromIntegral yS
                      
plotTrails :: (Integral a) => (a, a) -> [[(a, a)]] -> QDiagram SVG R2 Any
plotTrails size trails = (trailD `atop` borderD size) # centerXY # pad 1.1
  where trailD = mconcat $ map (testTrailD size) trails

-- testOutputD :: Grid -> QDiagram SVG R2 Any
testOutputD :: (Grid -> [[Coord]]) -> Grid -> QDiagram SVG R2 Any
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
testGrid14 = Grid (9,9) $ take 5
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
main = genOutputF testGrid14d
main1 = defaultMain (plotTrails (8,8) [map fst 
          [((5,4),2),((5,3),2),((5,2),2),((5,1),1),((6,1),1),((7,1),0),((7,2),3),((6,2),0),((6,3),0),((6,4),0),((6,5),0),((6,6),1),((7,6),0),((7,7),3),((6,7),3),((5,7),2),((5,6),2),((5,5),3),((4,5),3),((3,5),2),((3,4),3),((2,4),3),((1,4),2),((1,3),2),((1,2),1),((2,2),0),((2,3),1),((3,3),2),((3,2),2),((3,1),1)]
          ])
-- :main -o test2.svg -w 400
 