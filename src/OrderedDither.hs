module OrderedDither where
import ParsePPM

import Data.List.Split

thresholdMatrix = cycle [cycle [0.2, 0.8], cycle [0.6, 0.4]]

zip2d xs inf = if not (null xs) then (zip (head xs) (head inf)) : (zip2d (tail xs) (tail inf)) else []

orderedDither :: PPM -> PBM
orderedDither (PPM (Header f w h d) pixels) = PBM w h (concat $ toBW (threshold arr))
  where
    arr = chunksOf w (map grayscale pixels)
    grayscale (Pixel r g b) = (fromIntegral (r + g + b)) / (3 * (fromIntegral d))
    threshold ps = zip2d ps thresholdMatrix
    toBW = map (map (\(x, y) -> if x > y then Black else White)) 
