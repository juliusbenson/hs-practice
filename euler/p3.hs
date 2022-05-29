-- Largest prime factor
-- Problem 3
-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

-- n = 600851475143
divs n = takeWhile (\x -> x < (div n x)) [x | x <- [2..(div n 2)], mod n x == 0]
moreDivs n = d ++ map (div n) d
    where d = divs n
-- divs = takeWhile (\x -> x > div n x) (takeWhile (\x -> mod n x == 0) [2..(div n 2)])
-- divs n = takeWhile (\x -> mod n x == 0) [2..(div n 2)]
-- facs = takeWhile ((==0) . (mod n)) [2..(div n 2)]

-- cltN int = takeWhile ((<= int) . (^3)) [0..]