module Main where

import qualified Compiler as C
import qualified Simulation as S
import qualified Visualization as V

main :: IO ()
main = do
  microcode <- C.compileAufgabe6
  case microcode of
    Just m -> S.simulateAufgabe6 m >>= V.visualize
    Nothing -> return ()
