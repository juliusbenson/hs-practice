module Baby where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Geometry
import System.IO
import System.Random
import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Ratio

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- data Person = Person String String Int Float String String deriving (Show)
data Person = Person { firstName    :: String
                     , lastName     :: String
                     , age          :: Int
--                     , height       :: Float
--                     , phoneNumber  :: String
--                     , flavor       :: String
                     } deriving (Eq, Show, Read)

data Car = Car { company  :: String
               , model    :: String
               , year     :: Int
               } deriving (Show)

data Vector a = Vector a a a deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data LockerState = Taken | Free deriving (Show, Eq)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
instance Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = foldMap f l `mappend`
                             f x         `mappend`
                             foldMap f r

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red ="Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where
    yesno = id
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)
instance Tofu Frank where
    tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba :: t k}
instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

type AssocList k v = [(k,v)]

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
    if x > 100
    then x
    else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors' (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

tell' :: (Show a) => [a] -> String
tell' [] = "The list is empty"
tell' (x:[]) = "The list has one element: " ++ show x
tell' (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell' (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) =
    "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= under = "Underweight"
    | bmi <= mid   = "Midweight"
    | bmi <= over  = "Overweight"
    | True         = "Obese"
    where   bmi = weight / height ^ 2
            (under, mid, over) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | True  = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
    | a > b  = GT
    | a == b = EQ
    | True   = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
--calcBmis xs = [bmi w h | (w,h) <- xs]
--    where bmi weight height = weight / height ^ 2
calcBmis xs = [bmi|(w,h)<-xs,let bmi=w/h^2,bmi>=25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of [] -> error "undefined for empty list"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs =
    "The list is "++case xs of []  -> "empty"
                               [x] -> "singleton"
                               xs  -> "multi-element"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
--    | x > maxTail = x
--    | True        = maxTail
--    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | True   = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | True   = elem' a xs

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) =
--     let smaller = quicksort (filter (<=x) xs)
--         bigger  = quicksort (filter (> x) xs)
--     in smaller ++ [x] ++ bigger

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x  = x : filter p xs
    | True =     filter p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort (filter (<=x) xs)
        bigger  = quicksort (filter (> x) xs)
    in smaller ++ [x] ++ bigger

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = mod x 3829 == 0

chain :: (Integral a ) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (div n 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sumv2 :: (Num a) => [a] -> a
sumv2 = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

mapv2 :: (a -> b) -> [a] -> [b]
mapv2 f xs = foldr (\x acc -> f x : acc) [] xs

mapv3 :: (a -> b) -> [a] -> [b]
mapv3 f xs = foldl (\acc x -> acc ++ [f x]) [] xs

sumv3 :: (Num a) => [a] -> a
sumv3 = foldl1 (+)

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filterv2 :: (a -> Bool) -> [a] -> [a]
filterv2 p = foldr (\x acc -> if p x then x : acc else acc) []

headv2 :: [a] -> a
headv2 = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

-- findKey :: (Eq k) => k -> [(k,v)] -> v
-- findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k,v):xs) =
--     if key == k
--     then Just v
--     else findKey key xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
-- phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * (abs (y2 - y1))

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vector i j k) (Vector l m n) = i*l + j*m + k*n

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = elem (name, pnumber) pbook

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left ("Locker number " ++ show lockerNumber ++ " doesn't exist!")
        Just (state, code) -> if state /= Taken
            then Right code
            else Left ("Locker number " ++ show lockerNumber ++ " is already taken!")

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

yesnoif :: (YesNo y) => y -> a -> a -> a
yesnoif yesnoval yesresult noresult = if yesno yesnoval then yesresult else noresult

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
-- finiteRandoms 0 gen = ([], gen)
-- finiteRandoms n gen =
--     let (value, newGen) = random gen
--         (xs, finalGen) = finiteRandoms (n-1) newGen
--     in  (value:xs, finalGen)

solveRPN :: String -> Float
solveRPN = head . foldl foldFx [] . words
    where   foldFx (x:y:ys) "*" = (x * y):ys
            foldFx (x:y:ys) "+" = (x + y):ys
            foldFx (x:y:ys) "-" = (y - x):ys
            foldFx (x:y:ys) "/" = (y / x):ys
            foldFx (x:y:ys) "^" = (y ** x):ys
            foldFx (x:xs) "ln" = log x:xs
            foldFx xs "sum" = [sum xs]
            foldFx xs item = read item:xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA' [] = pure []
-- sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
sequenceA' = foldr (liftA2 (:)) (pure [])

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = compare (length x) (length y)
--                         b = compare x y
--                     in  if a == EQ then b else a
lengthCompare x y = mappend (compare (length x) (length y)) (compare x y)

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
-- landLeft n (left,right) = (left + n,right)
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
-- landRight n (left,right) = (left,right + n)
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- isBigGang :: Int -> Bool
-- isBigGang x = x > 9

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,mappend log newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["multiplying..."]
    return (a*b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0    = do
        tell ["result: " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
    mempty = DiffList (id)

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
    | b == 0 = do
        tell (toDiffList ["result: " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

slowCountDown :: Int -> Writer [String] ()
slowCountDown 0 = do
    tell ["0"]
slowCountDown x = do
    slowCountDown (x-1)
    tell [show x]

addStuff :: Int -> Int
-- addStuff = do
--     a <- (*2)
--     b <- (+10)
--     return (a+b)
addStuff x = let
    a = (*2) x
    b = (+10) x
    in a+b

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x,xs)

-- push :: Int -> Stack -> ((),Stack)
-- push a xs = ((),a:xs)

-- stackManip :: Stack -> (Int, Stack)
-- -- stackManip stack = let
-- --     ((),newStack1) = push 3 stack
-- --     (a ,newStack2) = pop newStack1
-- --     in pop newStack2
-- stackManip = do
--     push 3
--     a <- pop
--     pop

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins' :: State StdGen (Bool,Bool,Bool)
threeCoins' = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)

-- m >>= f = join (fmap f m)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["keeping " ++ show x]
        return True
    | otherwise = do
        tell ["removing " ++ show x]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True,False]) xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

rpn :: String -> Maybe Double
rpn st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [( Prob [('a',1%2),('b',1%2)] , 1%4)
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Applicative Prob where
    pure = return
    (<*>) = ap

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' EmptyTree EmptyTree)
                (Node 'T' EmptyTree EmptyTree)
            )
            (Node 'Y'
                (Node 'S' EmptyTree EmptyTree)
                (Node 'A' EmptyTree EmptyTree)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' EmptyTree EmptyTree)
                (Node 'R' EmptyTree EmptyTree)
            )
            (Node 'A'
                (Node 'A' EmptyTree EmptyTree)
                (Node 'C' EmptyTree EmptyTree)
            )
        )

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

-- type Breadcrumbs = [Direction]
type Breadcrumbs a = [Crumb a]

-- goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goLeft (Node _ l _,bs) = (l,L:bs)

-- goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goRight (Node _ _ r,bs) = (r,R:bs)

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r,bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r,bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)

modifyTree :: (a -> a) -> Zipper a -> Zipper a
modifyTree f (Node x l r, bs) = (Node (f x) l r, bs)
modifyTree f (EmptyTree, bs) = (EmptyTree, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t,bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost (goUp z)

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename:: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) =
    (Folder folderName (item:items), bs)

goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft' (EmptyTree, _) = Nothing

goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight' (EmptyTree, _) = Nothing

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing