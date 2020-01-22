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

-- pack (x:xs) = let (first,rest) = span (==x) xs
--                in (x:first) : pack rest

encode :: (Eq a) => [a] -> [(Int,a)]
encode x = zip (map (length) (pack x)) (compress x)

