import qualified Data.Map as Map

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
			then x
			else x*2
boomBang xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]


sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not a number between 1 and 5!!!"


addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
				| bmi <= 18.5 = "You are underweight"
				| bmi <= 25.0 = "You are normal"
				| bmi <= 30.0 = "You are overweight"
				| otherwise = "You are morbidly obese"
				where bmi = weight/height ^ 2

head' xs = case xs of [] -> error "No head for empty lists!"
		      (x:_) -> x

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st , c `elem` ['A'..'Z'] ]


addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

maximum' [] = error "List is empty!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


replicate' n i
	| n <= 0 = []
	| otherwise = i:replicate' (n-1) i


take' :: (Num i,Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

reverse' :: [a] -> [a]

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
				| x == a = True
				| otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerHead = quicksort [ a | a <- xs , a <= x ]
	    biggerHead = quicksort [ a | a <- xs , a > x ]
	in smallerHead ++ [x] ++ biggerHead

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
		| even n = n: chain (n `div` 2)
		| odd n = n : chain (3*n + 1)

numChains :: Int
numChains = length (filter isLong (map chain [1..100]))
 										where isLong xs = length xs > 15
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 -y1)

data Person = Person { firstName :: String , lastName :: String , age :: Int,height :: Float , phoneNumber :: String,flavour :: String } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vmultiply :: (Num t) => Vector t -> t -> Vector t
vmultiply (Vector i j k) s = Vector (i*s) (j*s) (k*s)

scalarMulti :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMulti` (Vector l m n) = (i*l) + (j*m) + (k*n)


data LockerState = Free | Taken deriving (Show,Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState,Code)


lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp num map=
	case Map.lookup num map of
		Nothing -> Left $ "Locker number " ++ show num ++ " doesn't exist"
		Just (state,code) -> if state /= Taken
												then Right $ "Locker Number " ++ code
												else Left $ "Locker number " ++ show num ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- Tree Structures!

--Recursive Structures and one line declaration of creating a binary search tree.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Eq,Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert num EmptyTree = singleton num
treeInsert x (Node a left right)
				| x == a = Node x left right
				| x < a  = Node a (treeInsert x left) right
				| x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem num EmptyTree = False
treeElem x (Node a left right)
				| x == a = True
				| x < a = treeElem x left
				| x > a = treeElem x right

-- Type Classes and Functors!
instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
