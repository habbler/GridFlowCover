{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} GridTest

main :: IO()
main = htfMain htf_importedTests
