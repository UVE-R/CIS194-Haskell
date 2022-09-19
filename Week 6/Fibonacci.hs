{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- generate nth fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- generate infinite list of fibonacci numbers - exponential time
fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-----------------------------------------------------------------------------

-- Generate infinite list of fibonacci numbers 

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

test :: Integer -> Integer
test x = snd $ foldl' (\acc _ -> (snd acc, snd acc + fst acc)) (0,1) [2..x]

fibs2 :: [Integer]
fibs2 = [0,1] ++ [test x | x <- [2..]]


fibs3 :: [Integer]
fibs3 = map fst (iterate f (0,1)) where f (x,y) = (y,x+y)

fibs4Helper :: Int -> [Integer] -> Integer
fibs4Helper 0 _ = 0
fibs4Helper 1 _ = 1
fibs4Helper idx lst = lst !! (idx -1) + lst !! (idx -2)

fibs4 :: [Integer]
fibs4 = [fibs4Helper x fibs3 | x <- [0..]]

fibs5 :: [Integer]
fibs5 = 0 : 1 : zipWith (+) fibs5 (tail fibs5)

-----------------------------------------------------------------------------

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show :: Show a => Stream a -> String
    show = show . take 20 . streamToList


streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

-----------------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-----------------------------------------------------------------------------

-- Stream of the natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams' :: Stream a -> Stream a -> Stream a
interleaveStreams' (Stream x xs) (Stream y ys) = Stream x (Stream y (interleaveStreams xs ys))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (streamRepeat <$> [0..])

-----------------------------------------------------------------------------

x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger :: Integer -> Stream Integer
    fromInteger x = Stream x $ streamRepeat 0

    negate :: Stream Integer -> Stream Integer
    negate = streamMap negate

    (+) :: Stream Integer -> Stream Integer -> Stream Integer
    (+) (Stream x xs) (Stream y ys) = Stream (x+y) ( xs + ys )

    (*) :: Stream Integer -> Stream Integer -> Stream Integer
    (*) (Stream x xs) z@(Stream y ys) = Stream (x * y) ( streamMap (* x) ys + xs * z )

instance Fractional (Stream Integer) where

    (/) :: Stream Integer -> Stream Integer -> Stream Integer
    (/) a@(Stream x xs) b@(Stream y ys) = Stream (x `div` y) (streamMap (`div` y) (xs - (a/b) * ys))


fibs6 :: Stream Integer
fibs6 = x / (1 - x - x^2)

-----------------------------------------------------------------------------

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) :: Matrix -> Matrix -> Matrix
    (*) (Matrix a b c d) (Matrix w x y z) = Matrix (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)

fMat :: Matrix
fMat = Matrix 1 1 1 0

getFn :: Matrix -> Integer
getFn (Matrix _ x _ _) = x

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getFn $ fMat ^ n

