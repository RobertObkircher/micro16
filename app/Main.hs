module Main where

import qualified Compiler as C
import qualified Simulator as S
import qualified Visualizer as V

main :: IO ()
main = C.someFunc -- >> V.visualize
