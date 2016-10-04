module Main where

-- outside modules
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- my modules
import Solver (computeAll)
import Plotter (plotAll)

main :: IO ()
main = do
    measureTime "Computation" computeAll
    measureTime "Plotting"    plotAll


measureTime :: String -> IO () -> IO ()
measureTime actionName action = do
  printf "Starting %s\n" actionName
  start <- getCPUTime
  action
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "%s time: %0.3f \n" actionName (diff :: Double)

