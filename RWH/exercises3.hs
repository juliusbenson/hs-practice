import Data.List

-- 1.Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.
elements (_:xs) = 1 + elements xs
elements _ = 0

-- 2.Add a type signature for your function to your source file. To test it, load the source file into ghci again.

-- 3.Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)
mean xs = (sum xs) / (fromIntegral $ length xs)

-- 4.Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
palindromify xs = xs ++ (reverse xs)

-- 5.Write a function that determines whether its input list is a palindrome.
palindrome xs = xs == (reverse xs)

-- 6.Create a function that sorts a list of lists based on the length of each sublist. (You may want to look at the sortBy function from the Data.List module.)
mySort [] = []
mySort (x:xs) = (mySort left) ++ [x] ++ (mySort right)
    where left  = filter (<= x) xs
          right = filter (> x) xs

-- 7.Define a function that joins a list of lists together using a separator value.
join _ (x:[]) = x
join c (x:xs) = x ++ c:(join c xs)

-- 8.Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty. For example, the tree Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height Empty = 0
height (Node _ l r) = 1 + (max rh lh)
    where rh = height r
          lh = height l

-- 9.Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it either turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
data Direction = LeftTurn | RightTurn | Straight deriving (Show)

-- 10.Write a function that calculates the turn made by three 2D points and returns a Direction.
direction (x1,y1) (x2,y2) (x3,y3) = case directionHelper (x1,y1) (x2,y2) (x3,y3) of
    LT -> LeftTurn
    GT -> RightTurn
    EQ -> Straight

directionHelper (x1,y1) (x2,y2) (x3,y3)
    | (x1 == x2) && (y1 < y2) = compare x3 x1
    | (x1 == x2) && (y1 > y2) = compare x1 x3
    | x1 < x2 = compare (ab x3) y3
    | x1 > x2 = compare y3 (ab x3)
    where ab x = ((y2 - y1) / (x2 - x1)) * (x - x1) + y1

-- 11.Define a function that takes a list of 2D points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction.
directions (x:y:z:zs) = (direction x y z):(directions (y:z:zs))
directions _ = []

-- 12.Using the code from the preceding three exercises, implement Graham's scan algorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull. is, and how the Graham scan algorithm should work, on Wikipedia.

sortSlopes xs = sortSlopesOn bottomLeft remaining
    where bottomLeft = (head (leftMost (bottomMost xs)))
          remaining = delete bottomLeft xs

sortSlopesOn _ [] = []
sortSlopesOn p (t:ts) = (sortSlopesOn p left) ++ [t] ++ (sortSlopesOn p right)
    where left  = filter (\l -> (slope p l) <= (slope p t)) ts
          right = filter (\r -> (slope p r) > (slope p t)) ts

slope (x0,y0) (x,y) = (y-y0)/(x-x0)

leftMost xs = filter (\t -> (fst t) == (minimum (map (fst) xs))) xs

bottomMost xs = filter (\t -> (snd t) == (minimum (map (snd) xs))) xs

-- now once we've sorted the slopes, we have to go through in order and find the direction.
-- if it's a right turn, we discard the middle and try again. (recursive call w/o 2nd)
-- if it's left, we move on to the next

graham xs = bottomLeft:(unsortedGraham (sortSlopes xs))
    where bottomLeft = (head (leftMost (bottomMost xs)))

unsortedGraham (a:b:[]) = a:b:[]
unsortedGraham (a:b:xs) = case (direction a b (head xs)) of
    RightTurn -> unsortedGraham (a:xs)
    otherwise -> a:(unsortedGraham (b:xs))