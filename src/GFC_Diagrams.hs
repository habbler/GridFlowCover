module GFC_Diagrams where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Text.Blaze.Svg.Renderer.Utf8

import FindFlowCover

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

testOutputD :: Grid -> QDiagram SVG R2 Any
testOutputD grid = plotTrails (gSize grid) (trailGrid grid)
        
genOuput :: Grid -> IO ()
genOuput test = defaultMain (testOutputD test)

testGrid9 = Grid (8,8) [EndPoints (2,0) (3,0), EndPoints (4,0) (5,0), EndPoints (1,0) (6,0),EndPoints (0,0) (7,0) ]
testGrid9a = Grid (8,8) [EndPoints (1,0) (6,0), EndPoints (2,1) (3,1), EndPoints (0,0) (7,0), EndPoints (4,1) (5,1) ]
testGrid12 = Grid (8,8) [EndPoints (2,0) (3,2), EndPoints (4,4) (5,6)] --, EndPoints (1,3) (6,7),EndPoints (0,0) (7,0) ]

-- out1 = renderSvg $ renderDia SVG (SVGOptions (Dims 200 200) Nothing) (testOutputD testGrid1)        

--main :: IO () 
main = genOuput testGrid9a
main1 = defaultMain (plotTrails (8,8) [map fst 
          [((4,0),0),((4,1),0),((4,2),1),((5,2),1),((6,2),0),((6,3),0),((6,4),0),((6,5),3),((5,5),3),((4,5),3),((3,5),0),((3,6),3),((2,6),2),((2,5),3),((1,5),2),((1,4),1),((2,4),2),((2,3),3),((1,3),2),((1,2),2)]
          ])
-- :main -o test2.svg -w 400
 