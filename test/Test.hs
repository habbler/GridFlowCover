module Test where

import FindFlowCover
import GFC_Diagrams

testGrid = Grid (2,2) [EndPoints (0,0) (0,1) ]
testGrid1 = Grid (5,4) [EndPoints (0,0) (4,3) ]
testGrid2 = Grid (7,6) [EndPoints (0,0) (6,5) ]
testGrid3 = Grid (11,10) [EndPoints (0,0) (10,9) ]
testGrid4 = Grid (13,12) [EndPoints (0,0) (12,11) ]
testGrid5 = Grid (2,2) [EndPoints (0,0) (0,1), EndPoints (1,0) (1,1)]
testGrid6 = Grid (6,6) [EndPoints (0,0) (0,5), EndPoints (5,0) (5,3)]
testGrid7 = Grid (6,6) [EndPoints (0,0) (4,4), EndPoints (4,1) (0,5)]
testGrid8 = Grid (8,8) [EndPoints (0,0) (0,7), EndPoints (5,0) (7,7)]
testGrid9 = Grid (8,8) [EndPoints (2,0) (3,0), EndPoints (4,0) (5,0), EndPoints (1,0) (6,0),EndPoints (0,0) (7,0) ]
testGrid10 = Grid (6,6) [EndPoints (2,0) (3,0), EndPoints (1,0) (4,0), EndPoints (0,0) (5,0) ]
testGrid11 = Grid (8,8) [EndPoints (2,0) (3,1), EndPoints (4,4) (5,5), EndPoints (1,3) (6,7),EndPoints (0,0) (7,0) ]
testGrid12 = Grid (8,8) [EndPoints (2,0) (3,1), EndPoints (4,4) (5,5)] --, EndPoints (1,3) (6,7),EndPoints (0,0) (7,0) ]

-- out1 = renderSvg $ renderDia SVG (SVGOptions (Dims 200 200) Nothing) (testOutputD testGrid1)        

--main :: IO () 
main = genOuput testGrid12 
-- :main -o test.svg -w 400
