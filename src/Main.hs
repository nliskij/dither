module Main where
import ParsePPM
import OrderedDither

bw :: Pixel -> Pixel
bw (Pixel r g b) = Pixel (fromEnum (r > 128) * 255) (fromEnum (g > 128) * 255) (fromEnum (b > 128) * 255)

main :: IO ()
main = do
  ppm <- readBitMap "examples/Balls.ppm"
  writeBitMap "examples/BWBalls.ppm" (orderedDither bw ppm)
