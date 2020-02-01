module H99 where

import System.Random as R

myLast :: (Eq a) => [a] -> a
myLast (x:[]) = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []     = True
isPalindrome [x]    = True
isPalindrome (x:xs) = if (x == (myLast xs))
    then isPalindrome (init xs)
    else False

-- isPalindrome xs = xs == (myReverse xs)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: (Eq a) => [a] -> [a]
compress [x] = [x]
compress (x:xs) = if x == head xs
    then compress xs
    else x:compress xs

pack :: (Eq a) => [a] -> [[a]]
pack [x] = [[x]]
pack (x:xs) = if x == head xs
    then (x:(head (pack xs))):(tail (pack xs))
    else [x]:(pack xs)

-- pack (x:xs) = (x:takeWhile (==x) xs):pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = zip (map (length) (pack xs)) (compress xs)

data RunLength a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified []     = []
encodeModified (x:xs) = if (takeWhile (==x) xs) == []
    then (Single x):encodeModified xs
    else (Multiple (length (takeWhile (==x) xs)) x):encodeModified (dropWhile (==x) xs)

decodeModified :: [RunLength a] -> [a]
decodeModified [] = []
decodeModified (Single a:xs) = [a] ++ decodeModified xs
decodeModified (Multiple n a:xs) = (take n (repeat a)) ++ decodeModified xs

encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect = encodeModified

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = take n (repeat x) ++ repli xs n

drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = if 0 == mod (length xs) n
    then (drop' (init xs) n)
    else (drop' (init xs) n) ++ (last xs):[]

split' :: [a] -> Int -> ([a],[a])
split' (_:xs) 0 = ([],xs)
split' (x:xs) n = let (t,u) = split' xs (n-1)
    in (x:t,u)

slice :: [a] -> Int -> Int -> [a]
slice (x:_)  _ 1 = x:[]
slice (x:xs) 1 m = x:(slice xs 1 (m-1))
slice (_:xs) n m = (slice xs (n-1) (m-1))

rotL :: [a] -> [a]
rotL (x:xs) = xs ++ [x]

rotR :: [a] -> [a]
rotR xs = (last xs):(init xs)

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n = if n >= 0
    then rotate (rotL xs) (n-1)
    else rotate (rotR xs) (n+1)

removeAt :: Int -> [a] -> (a,[a])
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = let (t,u) = removeAt (n-1) xs
    in (t,x:u)

removeAt' :: Int -> [a] -> (a,[a])
removeAt' 0 (x:xs) = (x,xs)
removeAt' n (x:xs) = let (t,u) = removeAt' (n-1) xs
    in (t,x:u)

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 1 = x:ys
insertAt x (y:ys) n = y:(insertAt x ys (n-1))

range :: Int -> Int -> [Int]
range n m = if n == m
    then n:[]
    else n:(range (n+1) m)

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return (take n (rands xs gen))

rands :: [a] -> StdGen -> [a]
rands xs gen = let (i, newGen) = (random gen :: (Int,StdGen))
    in (xs!!(mod i (length xs))):(rands xs newGen)

diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select [1..m] n

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
    gen <- getStdGen
    return (permu xs gen)

permu :: [a] -> StdGen -> [a]
permu [] _ = []
permu xs gen =
    let (i, newGen) = (random gen :: (Int,StdGen))
        (t,remaining) = removeAt' (mod i (length xs)) xs
    in t:(permu remaining newGen)

-- unoriginal code
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
    i <- [0..(length xs)-1]
    x <- combinations (n-1) (drop (i+1) xs)
    return (xs !! i : x)
-- original code

combinations' :: Int -> [a] -> [([a],[a])]
combinations' 0 _ = [([],[])]
combinations' n xs = do
    i     <- [0..(length xs)-1]
    (x,t) <- combinations' (n-1) (drop (i+1) xs)
    return (xs !! i : x, take (i+1) xs)

group :: [Int] -> [String] -> [[[String]]]
group [] _ = [[]]
group (n:ns) xs = do
    (x,remaining) <- combinations' n xs
    y <- group ns remaining
    return (x:y)

group = do
    subset <- 
    disjointsubset <-
    setofdisjointsubsets <- 
    return setofdisjointsubsets

