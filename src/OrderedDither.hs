module OrderedDither where
import ParsePPM

import Data.List.Split

thresholdMatrix = cycle $ map cycle [[5.8823529411764705e-2,0.5294117647058824,0.17647058823529413,0.6470588235294118],[0.7647058823529411,0.29411764705882354,0.8823529411764706,0.4117647058823529],[0.23529411764705882,0.7058823529411765,0.11764705882352941,0.5882352941176471],[0.9411764705882353,0.47058823529411764,0.8235294117647058,0.35294117647058826]]

zip2d xs inf = if not (null xs) then (zip (head xs) (head inf)) : (zip2d (tail xs) (tail inf)) else []

orderedDither :: PPM -> PBM
orderedDither (PPM (Header f w h d) pixels) = PBM w h (concat $ toBW (threshold arr))
  where
    arr = chunksOf w (map grayscale pixels)
    grayscale (Pixel r g b) = (fromIntegral (r + g + b)) / (3 * (fromIntegral d))
    threshold ps = zip2d ps thresholdMatrix
    toBW = map (map (\(x, y) -> if x > y then Black else White)) 
