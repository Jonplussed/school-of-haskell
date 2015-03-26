module HW3.Golf
( skips
, localMaxima
, histogram
) where

import Data.List
import Data.Char

skips :: [a] -> [[a]]
skips list = zipWith (\n _ -> every n list) [1..] list

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) =
    if y > x && y > z
    then y : localMaxima (z:zs)
    else localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram = unlines . transpose . histColumns . map fromIntegral

-- private

every :: Integer -> [a] -> [a]
every n list = every' n list
  where
    every' _ []     = []
    every' 1 (x:xs) = x : every' n xs
    every' r (_:xs) = every' (r - 1) xs

histColumn :: Int -> (Int, Int) -> String
histColumn rows (n, count) =
    replicate (rows - count) ' ' ++
    replicate count '*' ++
    ['=', intToDigit  n]

histColumns :: [Int] -> [String]
histColumns ns = map column $ zip histRng counts
  where
    column = histColumn $ maximum counts
    counts = histCounts ns

histRng :: [Int]
histRng = [0..9]

histCounts :: [Int] -> [Int]
histCounts = map count . group . sort . (histRng ++)
  where
    count ns = length ns - 1
