{-# LANGUAGE BangPatterns #-}

import Data.List (nub, (\\), sortBy, isPrefixOf)
import Data.Char
import Data.Ord

findOdd :: [Int] -> Int
findOdd (x:xs) = if even (length (filter (x==) xs))
                  then x
                  else findOdd (filter (x/=) xs)
findOdd [] = 0

findNb :: Integer -> Integer
findNb = helper 1
  where
    helper !n !m
      | m == 0 = 0
      | m > 0 = 1 + helper (n + 1) (m - (n ^ 3))
      | otherwise = -n

persistence :: Int -> Int
persistence n = if div n 10 == 0
                then 0
                else 1 + persistence (product $ digits n)

-- digits :: Int -> [Int]
-- digits 0 = []
-- digits n = mod n 10 : digits (div n 10)

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers r = sum $ take (fromIntegral r) [n,m..]
  where n = r * (r-1) + 1
        m = r * (r-1) + 3

humanReadable :: Int -> String
humanReadable x =
    show' (div x (60*60)) ++ ":" ++
    show' (div (mod x (60*60)) 60) ++ ":" ++
    show' (mod (mod x (60*60)) 60)

show' n = if n < 10 then "0" ++ show n else show n

unsum :: Int -> [Int]
unsum 0 = []
unsum n = mag * d :unsum m
    where
        mag = 10 ^ floor (logBase (fromIntegral 10) (fromIntegral n))
        (d, m) = divMod n mag

validBraces :: String -> Bool
validBraces [] = True
validBraces xs = (length x < length xs) && validBraces x
  where x = remove "()" $ remove "[]" $ remove "{}" xs

remove _ [] = []
remove x yss@(y:ys) = if x `isPrefixOf` yss
                      then remove x (drop (length x) yss)
                      else y:remove x ys

-- isValidWalk :: [Char] -> Bool
-- isValidWalk walk = length walk == 10 && ns == ss && es == ws
--   where ns = length $ filter ('n' ==) walk
--         ss = length $ filter ('s' ==) walk
--         es = length $ filter ('e' ==) walk
--         ws = length $ filter ('w' ==) walk

isValidWalk :: [Char] -> Bool
isValidWalk walk = n+s+e+w == 10 && n == s && e == w
    where (n,s,e,w) = nsewTuple walk

nsewTuple :: String -> (Int,Int,Int,Int)
nsewTuple [] = (0,0,0,0)
nsewTuple (x:xs)
    | x == 'n' = (n+1,s,e,w)
    | x == 's' = (n,s+1,e,w)
    | x == 'e' = (n,s,e+1,w)
    | x == 'w' = (n,s,e,w+1)
    | otherwise = error "invalid direction"
    where (!n,!s,!e,!w) = nsewTuple xs

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (\x y -> compare (filter isDigit x) (filter isDigit y)) . words

getUnique :: [Int] -> Int
getUnique xs = head $ dropWhile (== head (xs \\ nub xs)) xs

getPINs :: String -> [String]
getPINs [] = [[]]
getPINs (x:xs) = [ y:ys | y <- adjacents x, ys <- getPINs xs ]

adjacents '1' = "124"
adjacents '2' = "1235"
adjacents '3' = "236"
adjacents '4' = "1457"
adjacents '5' = "24568"
adjacents '6' = "3569"
adjacents '7' = "478"
adjacents '8' = "57890"
adjacents '9' = "689"
adjacents '0' = "80"
adjacents _   = error "invalid digit"

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
    | interesting x xs == Yes = Yes
    | interesting (x+1) xs == Yes = Almost
    | interesting (x+2) xs == Yes = Almost
    | otherwise = No

interesting :: Integer -> [Integer] -> Answer
interesting x xs
    | x < 100             = No
    | followedByZero x    = Yes
    | monotone x          = Yes
    | palindrome x        = Yes
    | x `elem` xs         = Yes
    | incrementing x      = Yes
    | decrementing x      = Yes
    | otherwise           = No

followedByZero :: Integer -> Bool
followedByZero x = d == 0 && null ds
    where (d:ds) = nub $ tail $ digits x

monotone :: Integer -> Bool
monotone = (==1) . length . nub . digits

decrementing :: Integer -> Bool
decrementing x = digits x == take (length xs) [y,z..]
    where xs@(y:z:_) = digits x

-- eff this
incrementing :: Integer -> Bool
incrementing 123 = True
incrementing 234 = True
incrementing 345 = True
incrementing 456 = True
incrementing 567 = True
incrementing 678 = True
incrementing 789 = True
incrementing 890 = True
incrementing 1234 = True
incrementing 2345 = True
incrementing 3456 = True
incrementing 4567 = True
incrementing 5678 = True
incrementing 6789 = True
incrementing 7890 = True
incrementing 12345 = True
incrementing 23456 = True
incrementing 34567 = True
incrementing 45678 = True
incrementing 56789 = True
incrementing 67890 = True
incrementing 123456 = True
incrementing 234567 = True
incrementing 345678 = True
incrementing 456789 = True
incrementing 567890 = True
incrementing 1234567 = True
incrementing 2345678 = True
incrementing 3456789 = True
incrementing 4567890 = True
incrementing 12345678 = True
incrementing 23456789 = True
incrementing 34567890 = True
incrementing 123456789 = True
incrementing 234567890 = True
incrementing 1234567890 = True
incrementing _ = False

palindrome :: Integer -> Bool
palindrome x = digits x == reverse (digits x)

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- actual solution:
-- decrementing x = show x `isInfixOf` "9876543210"
-- incrementing x = show x `isInfixOf` "1234567890"

type Ratio a = (a, a) -- Data.Ratio not suitable for this kata

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs xs = map (\(n,d) -> (n * (lcd `div` d), lcd)) xs
    where lcd = foldr (lcm . snd) 1 xs

-- permutations :: String -> [String]
-- permutations a = heapPermutation (length a) a

-- heapPermutation :: Int -> [a] -> [[a]]
-- heapPermutation 1 a = [a]
-- heapPermutation size a = heapLoop size a
--     --perms <- heapPermutation a (size - 1)

-- heapLoop :: Int -> Int -> [a] -> [a]
-- heapLoop _ 0 _ = []
-- heapLoop s i a = do
--     perms <- heapPermutation (s-1) a
--     return heapLoop s (i-1) (swappy s i perms)
--     return perms

-- swappy :: Int -> Int -> [a] -> [a]
-- swappy s i a = swapLast h a
--     where h = if odd s then 0 else i

-- swapLast :: Int -> [a] -> [a]
-- swapLast i a = init b ++ last c : init c ++ last b : []
--     where (b,c) = splitAt i a

-- permutations :: String -> [String]
-- permutations = nub' . permutation

permutation [] = [[]]
permutation xs = do
    element <- xs
    follows <- permutation (delete' element xs)
    return (element : follows)

delete' a (x:xs)
    | a == x = xs
    | a /= x = x : delete' a xs
delete' _ _ = []

nub' (x:xs) = x : (nub' $ filter (/=x) xs)
nub' _ = []
