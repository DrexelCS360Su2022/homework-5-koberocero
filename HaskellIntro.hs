{-# OPTIONS_GHC -fwarn-tabs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HaskellIntro where

import Set
import System.Win32 (COORD(x))
import Data.Tree (flatten)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit num = num `rem` 10

dropLastDigit :: Integer -> Integer
dropLastDigit num = num `div` 10

toDigits :: Integer -> [Integer]
toDigits x = reverse(toDigitsHelper x)

toDigitsHelper :: Integer -> [Integer]
toDigitsHelper x | x <= 0 = []
toDigitsHelper x = lastDigit x : toDigitsHelper (dropLastDigit x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherHelper (reverse x))

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper [x] = [x]
doubleEveryOtherHelper (x:xs:ys) = x : xs*2 : doubleEveryOtherHelper ys

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = sumDigitList(head x) + sumDigits(drop 1 x)

sumDigitList :: Integer -> Integer
sumDigitList x = sum(toDigits x)

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

--
-- Problem 2 
--

pow :: (a -> a) -> Int -> a -> a
pow f 0 = id
pow f 1 = f
pow f n = f . pow f (n-1)


g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n-1)

h :: Integer -> Integer
h 0 = 0
h n = n - pow h 3 (n-1)

d :: Int -> Integer -> Integer
d i 0 = 0
d i n = n - pow (d i) i (n-1)


--
-- Problem 3
--

powerSet set    | isEmpty set = singleton empty
                | otherwise = union (mapSet (insert x) (powerSet t)) (powerSet t)
                 where t = snd (split set)
                       x = fst (split set)

