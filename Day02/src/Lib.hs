{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import           Data.List

someFunc :: IO ()
someFunc = do
        input <- lines <$> readFile "src/input.txt"
        print $ countPosition input

countPosition xs = (down - up) * forward
  where
    down = head result
    forward = head $ tail result
    up = head $ tail $ tail result
    result = yellow xs

yellow = map sum . map red . groupBy green . sort . map words

red = map $ (\x -> read x::Int) . head . tail

green a b = head a == head b
