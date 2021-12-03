module Lib
    ( someFunc
    ) where

import Data.List
import Data.Char (digitToInt)

someFunc :: IO ()
someFunc = do
        input <- lines <$> readFile "src/input.txt"
        print $ calculateAll input

calculateAll :: [String] -> Int
calculateAll xs = toDec oxygen * toDec scrubber
  where
    scrubber = filterMostCommon xs 0
    oxygen = filterLeastCommon xs 0

filterMostCommon :: [String] -> Int -> String
filterMostCommon = filterByFrequency mostCommon
filterLeastCommon :: [String] -> Int -> String
filterLeastCommon = filterByFrequency leastCommon

filterByFrequency _ [] _ = ""
filterByFrequency _ [x] _ = x
filterByFrequency frequency xs i = filterByFrequency frequency filtered (i+1)
  where
    predicates = map frequency $ transpose xs
    predicate = predicates !! i
    selectedPredicate x = (x !! i) == predicate
    filtered = filter selectedPredicate xs

mostCommon :: Ord a => [a] -> a
mostCommon = common maximum
leastCommon :: Ord a => [a] -> a
leastCommon = common minimum

common frequency = snd . frequency . map (\xs -> (length xs, head xs)) . group . sort

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
