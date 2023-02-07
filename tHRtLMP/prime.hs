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