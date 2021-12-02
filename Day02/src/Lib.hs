{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
        input <- lines <$> readFile "src/input.txt"
        print $ foldl move (0,0,0) $ map words input

move oldPosition command =
  adjustPosition direction value oldPosition
  where
    direction = head command
    value = (\x -> read x::Int) $ head $ tail command

adjustPosition "up" value (horizontal, depth, aim) = (horizontal, depth, aim - value)
adjustPosition "down" value (horizontal, depth, aim) = (horizontal, depth, aim + value)
adjustPosition "forward" value (horizontal, depth, aim) = (horizontal', depth', aim)
  where
    horizontal' = horizontal + value
    depth' = depth + (aim * value)
