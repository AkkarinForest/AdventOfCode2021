module Lib
    ( someFunc
    ) where

import Data.List
import Data.Char (digitToInt)

someFunc :: IO ()
someFunc = do
        input <- lines <$> readFile "src/input.txt"
        print $ calculatePower input

calculatePower xs = toDec epsilon * toDec gamma
  where
    gamma = map mostCommon $ transpose xs
    epsilon = map flipBit gamma

flipBit '0' = '1'
flipBit '1' = '0'

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
