{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

--Wholemeal programming

func1 :: [Integer] -> Integer
func1  = product . map ( subtract 2 ) . filter even

fun2Helper :: Integer -> Integer
fun2Helper n
       | even n = n `div` 2
       | otherwise = 3 * n + 1

func2 :: Integer -> Integer
func2 x = sum . filter even . takeWhile (>1) $ iterate fun2Helper x

------------------------------------------------------------------------------------------------------------------

-- Create a balanced binary tree with correct heights for each node

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
       deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight (Node height _ _ _) = height
getHeight Leaf = 0

addToTree :: a -> Tree a -> Tree a
addToTree x Leaf = Node 0 Leaf x Leaf
addToTree x y@(Node height left current right)
       | getHeight left < getHeight right = Node ( calcHeight y + 1) (addToTree x left) current right
       | otherwise = Node (calcHeight y + 1) left current (addToTree x right)

calcHeight :: Tree a -> Integer
calcHeight Leaf = 1
calcHeight (Node height left _ right) = max (getHeight left) (getHeight right)

foldTree :: [a] -> Tree a
foldTree = foldr addToTree Leaf

--------------------------------------------------------------------------------------------------------------

--True if odd number of True in bool list

xor :: [Bool] -> Bool
xor lst = odd $ foldr (\x acc -> if x then acc + 1 else acc) 0 lst

--------------------------------------------------------------------------------------------------------------

-- Map implementation using foldr

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-------------------------------------------------------------------------------------------------------------

--Foldl implementation using foldr

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

------------------------------------------------------------------------------------------------------------

-- Sieve of Sundaram

cartProd :: [Integer] -> [Integer] -> [Integer]
cartProd xs ys = [ x + y + 2* (x * y)| x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) $ filter (`notElem` ys) [1..n]
       where ys = cartProd [1..n] [1..n]