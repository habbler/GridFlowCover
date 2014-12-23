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

testOutputD :: Grid -> QDiagram SVG R2 Any
testOutputD grid = (trailD `atop` borderD size) # centerXY # pad 1.1
  where trailD = mconcat $ map (testTrailD size) $ trailGrid grid
        size = gSize grid
        
genOuput :: Grid -> IO ()
genOuput test = defaultMain (testOutputD test)

