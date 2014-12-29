module Main where

import FindTrails (Grid(Grid), EPTrail(EndPoints))

import GFC_Diagrams

main :: IO () 
main = genOutput testGrid12a

testGrid9a = Grid (8,8) [EndPoints (1,0) (6,0), EndPoints (2,1) (3,1), EndPoints (0,0) (7,0), EndPoints (4,1) (5,1) ]
testGrid12a = Grid (8,8) [EndPoints (2,0) (3,2), EndPoints (4,4) (5,5), EndPoints (1,3) (6,7),EndPoints (0,0) (7,0) ]

