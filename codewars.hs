{-# LANGUAGE BangPatterns #-}

module CodeWars where

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

digits :: Int -> [Int]
digits 0 = []
digits n = mod n 10 : digits (div n 10)

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers r = sum $ take (fromIntegral r) [n,m..]
  where n = r * (r-1) + 1
        m = r * (r-1) + 3

