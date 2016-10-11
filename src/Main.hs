module Main where
import ParsePPM
import OrderedDither
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  ppm <- readBitMap (head args)
  writePBM (last args) (orderedDither ppm)
