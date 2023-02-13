import Data.List

divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n   = k
        | k^2 > n       = n
        | otherwise     = ldf (k+1) n

prime0 :: Integer -> Bool
prime0 n    | n < 1     = error "not a positive integer"
            | n == 1    = False
            | otherwise = ld n == n

-- a fun foldable sieve of eratosthenes
-- not very safe, uses !!, prone to run out of bounds
sieve x i = (x \\ (map (*(x !! i)) x))

sub100 = foldl (sieve) [2..100] [0..24] -- for example

--1.8
mnmInt xs = foldl1 (min) xs

--1.9
max' :: (Foldable t, Ord a) => t a -> a
max' = foldl1 (max)

--1.10
removeFst m xs = xs \\ [m]

--1.11
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

--1.12
average xs = (sum xs) / fromIntegral (length xs)

--1.13
countAs :: Eq a => a -> [a] -> Int
countAs _ [] = 0
countAs a (x:xs) | a == x = 1 + countAs a xs
                 | otherwise =  countAs a xs

--1.14
bh n [x] = replicate n x
bh n (x:xs) = replicate (n - length xs) x ++ bh n xs

blowup :: [a] -> [a]
blowup xs = bh (length xs) xs

--1.15
srtString :: [String] -> [String]
srtString = sort

--1.16
prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

--1.17
substring :: String -> String -> Bool
substring [] [] = True
substring _ [] = False
substring xs ys@(_:ys') = prefix xs ys || substring xs ys'

factors :: Integer -> [Integer]
factors n
    | n < 1 = error "cannot factor a number less than 1"
    | n == 1 = []
    | otherwise = p : factors (div n p)
    where p = ld n

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map f xs)

--1.20
lengths :: Foldable t => [t a] -> [Int]
lengths = map length

--1.21
sumLengths :: Foldable t => [t a] -> Int
sumLengths = sum . lengths

--1.24

--experimenting for fun

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p 
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : filter (\n -> ldpf primes1 n == n) [3..]
