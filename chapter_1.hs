import Data.List

median :: [Double] -> Double
median [] = 0
median xs = if len `mod` 2 == 1 then middleOdd else middleEven
  where
    sorted = sort xs
    len = length xs
    middle = len `quot` 2
    middleOdd = sorted !! middle
    middleEven = (middleOdd + (sorted !! middle - 1)) / 2

vowelIndices :: String -> [Integer]
vowelIndices = fmap fst . filter (flip elem "aeiouAEIOU" . snd) . zip [1 ..]
