module Main where

import qualified Compiler as C
import qualified Simulator as S
import qualified Visualizer as V

main :: IO ()
main = do
  microcode <- C.compileAufgabe6
  case microcode of
    Just m -> S.simulateAufgabe6 m
    Nothing -> return ()
