module H99 where

import System.Random as R

myLast :: (Eq a) => [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast _ = error "empty list"

myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "list too short"

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt [] _ = error "index out of range"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []     = True
isPalindrome (x:xs) = (x == myLast xs) && isPalindrome (init xs)

-- isPalindrome xs = xs == (myReverse xs)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs
    then compress xs
    else x:compress xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs) = if x == head xs
    then (x : head (pack xs)) : tail (pack xs)
    else [x] : pack xs

-- pack (x:xs) = (x:takeWhile (==x) xs):pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = zip (map length (pack xs)) (compress xs)

data RunLength a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified []     = []
encodeModified (x:xs) = if null (takeWhile (== x) xs)
    then Single x : encodeModified xs
    else Multiple (length (takeWhile (==x) xs)) x:encodeModified (dropWhile (==x) xs)

decodeModified :: [RunLength a] -> [a]
decodeModified [] = []
decodeModified (Single a:xs) = a : decodeModified xs
decodeModified (Multiple n a:xs) = replicate n a ++ decodeModified xs

encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect = encodeModified

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = if 0 == mod (length xs) n
    then drop' (init xs) n
    else drop' (init xs) n ++ [last xs]

split' :: [a] -> Int -> ([a],[a])
split' [] _ = error "index out of range"
split' (_:xs) 0 = ([],xs)
split' (x:xs) n = let (t,u) = split' xs (n-1)
    in (x:t,u)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:_)  _ 1 = [x]
slice (x:xs) 1 m = x : slice xs 1 (m - 1)
slice (_:xs) n m = slice xs (n - 1) (m - 1)

rotL :: [a] -> [a]
rotL [] = []
rotL (x:xs) = xs ++ [x]

rotR :: [a] -> [a]
rotR xs = last xs : init xs

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n = if n >= 0
    then rotate (rotL xs) (n-1)
    else rotate (rotR xs) (n+1)

removeAt :: Int -> [a] -> (a,[a])
removeAt _ [] = error "index out of range"
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = let (t,u) = removeAt (n-1) xs
    in (t,x:u)

removeAt' :: Int -> [a] -> (a,[a])
removeAt' _ [] = error "index out of range"
removeAt' 0 (x:xs) = (x,xs)
removeAt' n (x:xs) = let (t,u) = removeAt' (n-1) xs
    in (t,x:u)

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x:ys
insertAt x (y:ys) n = y : insertAt x ys (n - 1)
insertAt _ _ _ = error "index out of range"

range :: Int -> Int -> [Int]
range n m = if n == m
    then [n]
    else n : range (n + 1) m

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = take n . rands xs <$> getStdGen

rands :: [a] -> StdGen -> [a]
rands xs gen = let (i, newGen) = (random gen :: (Int,StdGen))
    in (xs !! mod i (length xs)) : rands xs newGen

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

rndPermu :: [a] -> IO [a]
rndPermu xs = permu xs <$> getStdGen

permu :: [a] -> StdGen -> [a]
permu [] _ = []
permu xs gen =
    let (i, newGen) = (random gen :: (Int,StdGen))
        (t,remaining) = removeAt' (mod i (length xs)) xs
    in t : permu remaining newGen
