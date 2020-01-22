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

