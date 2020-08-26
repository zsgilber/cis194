module Homework1 where

--
-- Exercise 1
--

--  Convert positiveIntegers to a list of digits.
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n <= 9 = [n]
  | otherwise = toDigits (n `div` 10) <> [n `mod` 10]

-- Convert positiveIntegers to a reversed list of digits.
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

--
-- Exercise 2
--

-- Double every other integer in the list, starting from the left.
doubleEveryOtherFromFront :: [Integer] -> [Integer]
doubleEveryOtherFromFront [] = []
doubleEveryOtherFromFront [x] = [x]
doubleEveryOtherFromFront (x : (y : zs)) = x : 2 * y : doubleEveryOtherFromFront zs

-- Double every other integer in the list, starting from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromFront . reverse

--
-- Exercise 3
--

-- Sum over all of the digits in each Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ toDigits =<< xs

--
-- Exercise 4
--

-- Determine if the remainder of the integer is zero when dividing by 10.
zeroRemainder :: Integer -> Bool
zeroRemainder x = x `mod` 10 == 0

-- Validate that the integer represents a valid credit card number.
validate :: Integer -> Bool
validate = zeroRemainder . sumDigits . doubleEveryOther . toDigits
