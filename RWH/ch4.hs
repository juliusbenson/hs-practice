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