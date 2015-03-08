module HW1.CreditCard
( toDigits
, toDigitsRev
, doubleEveryOther
, sumDigits
, validate
) where

import Data.List (foldl', unfoldr)
import Data.Tuple (swap)

type CCNumber = Integer

toDigits :: CCNumber -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: CCNumber -> [Integer]
toDigitsRev = unfoldr nextDigit

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleOther . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldl' (\total -> (+) total . sum . toDigitsRev) 0

validate :: CCNumber -> Bool
validate ccn = remain == 0
  where
    (_,remain) =  ccnHash ccn `divMod` 10

-- private functions

nextDigit :: Integer -> Maybe (Integer, Integer)
nextDigit n =
    if n > 0
    then Just . swap $ n `divMod` 10
    else Nothing

doubleIfFlag :: (Bool, Integer) -> Integer
doubleIfFlag (flag, n) =
    if flag
    then 2 * n
    else n

doubleOther :: [Integer] -> [Integer]
doubleOther = zipWith (curry doubleIfFlag) (cycle [False, True])

ccnHash :: CCNumber -> Integer
ccnHash = sumDigits . doubleOther . toDigitsRev
