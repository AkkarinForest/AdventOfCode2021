module Lib
    ( someFunc
    ) where

import Data.List
import Data.Char (digitToInt)

someFunc :: IO ()
someFunc = do
        input <- lines <$> readFile "src/input.txt"
        print $ calculateAll input

yellow :: String -> [String] -> Int -> String
yellow predicates [] i = ""
yellow predicates [x] i = x
yellow predicates xs i = yellow newPredicates filtered (i+1)
  where
    a = predicates !! i
    green x = (x !! i) == a
    filtered = filter green xs
    newPredicates = map mostCommon $ transpose filtered
yellow2 :: String -> [String] -> Int -> String
yellow2 predicates [] i = ""
yellow2 predicates [x] i = x
yellow2 predicates xs i = yellow2 newPredicates filtered (i+1)
  where
    a = predicates !! i
    green x = (x !! i) == a
    filtered = filter green xs
    newPredicates = map flipBit $ map mostCommon $ transpose filtered

calculateAll xs = toDec oxygen * toDec scrubber
-- calculateOxygen xs = filter (\(x, row) ->head row == x) withMostCommon
  where
    scrubber = yellow2 epsilon xs 0
    oxygen = yellow gamma xs 0
    gamma = map mostCommon $ transpose xs
    epsilon = map flipBit gamma

flipBit '0' = '1'
flipBit '1' = '0'

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
