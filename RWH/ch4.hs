length' (_:xs) = 1 + length' xs
length' _ = 0

null' [] = True
null' _ = False

head' (x:_) = x

tail' (_:xs) = xs

last' (x:[]) = x
last' (_:xs) = last' xs

init' (_:[]) = []
init' (x:xs) = x:(init' xs)

(x:xs) +++ ys = x:(xs +++ ys)
[] +++ ys = ys

concat' (xs:[]) = xs
concat' (xs:xss) = xs +++ concat' xss

reverse' [] = []
reverse' xs = (last' xs):(reverse' (init' xs))

palindrome xs = xs == reverse' xs

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

and'' [] = True
and'' (x:xs) = and' x (and'' xs)

or'' [] = False
or'' (x:xs) = or' x (or'' xs)

all' f [] = True
all' f (x:xs) = and' (f x) (all' (f) xs)

any' f [] = False
any' f (x:xs) = or' (f x) (any' (f) xs)

take' 0 _ = []
take' n (x:xs) = x:(take (n-1) xs)

drop' 0 xs = xs
drop' n (x:xs) = drop (n-1) xs

splitAt' n xs = ((take' n xs),(drop' n xs))

takeWhile' _ [] = []
takeWhile' f (x:xs)
    | (f x)     = x:(takeWhile' f xs)
    | otherwise = []

dropWhile' _ [] = []
dropWhile' f all@(x:xs)
    | (f x)     = dropWhile' f xs
    | otherwise = all

span' f xs = ((takeWhile' f xs),(dropWhile' f xs))

break' f xs = span' (not . f) xs

elem' x xs = any' (x ==) xs

notElem' x xs = not (elem' x xs)

filter' _ [] = []
filter' f (x:xs)
    | (f x)     = x:(filter' f xs)
    | otherwise = filter' f xs

isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
    | x == y    = isPrefixOf' xs ys
    | otherwise = False

isInfixOf' _ [] = False
isInfixOf' xs (y:ys) = or' (isPrefixOf' xs (y:ys)) (isInfixOf' xs ys)

tails' [] = [[]]
tails' (x:xs) = (x:xs):(tails' xs)

isSuffixOf' xs ys = elem' xs (tails' ys)

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

lines' [] = [[]]
lines' xs = word:(lines' rs)
    where (word,(r:rs))) = span' ('\n' /=) xs

