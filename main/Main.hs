module Main where

import FindTrails (Grid(Grid), EPTrail(EndPoints))

import GFC_Diagrams

main :: IO () 
main = genOutputF testGrid9a

testGrid9a = Grid (8,8) [EndPoints (1,0) (6,0), EndPoints (2,1) (3,1), EndPoints (0,0) (7,0), EndPoints (4,1) (5,1) ]

