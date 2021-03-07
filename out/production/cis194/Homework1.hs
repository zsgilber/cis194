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


--
-- Exercise 5
--

type Peg = String
type Move = (Peg, Peg)
-- Calculate the moves to solve a 3 peg Tower of Hanoi.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n > 0 = hanoi (n-1) a c b <> [(a, b)] <> hanoi (n-1) c b a
  | otherwise = []

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = [(a, b)]
hanoi4 n a b c d =
  hanoi4 (n-k) a c b d <> hanoi k a b d <> hanoi4 (n-k) c b a d
  where k = 2

