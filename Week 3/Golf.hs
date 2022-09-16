module Golf where

import Data.List

-----------------------------------------------------------------------------------------------------------------------

-- Return every nth element from a list
takerHelp :: [a] -> Int -> Int -> [a]
takerHelp [] _ _ = []
takerHelp (x:xs) n pos
    | mod pos n == 0 = x : takerHelp xs n (pos+1)
    | otherwise = takerHelp xs n (pos+1)

-- Carry out all skips on lst where n represents the nth element
-- Stop once the nth element is out of bounds
doSkip :: [a] -> Int -> [[a]]
doSkip lst n
    | length lst < n  = []
    | otherwise = takerHelp lst n 1 : doSkip lst (n+1)

skips :: [a] -> [[a]]
skips [] = []
skips lst = doSkip lst 1

{-
-- Create a tuple of each individual element in the list coupled with its element number (element number, element) 
-- Then create a new list where the element number is divisible by the nth element number
-- Do this for nth elements from 1 to the length of the list
skips1 :: [a] -> [[a]]
skips1 xs = [ [ e | (i, e) <- zip [1..] xs, i `mod` n == 0 ] | n <- [1..length xs]]
-}

-----------------------------------------------------------------------------------------------------------------------

-- Return a list of the local maxima from the input list
localMaxima :: [Integer] -> [Integer]

localMaxima (x1: xs@(x2:x3:_))
    -- Check if the 2nd elements out of 3 is a local maxima
    -- Call the function again on the tail of the list
    | x2 > x1 && x2 > x3 = x2 : localMaxima xs
    | otherwise = localMaxima xs

-- Must be atleast 3 elements
localMaxima _ = []

-----------------------------------------------------------------------------------------------------------------------

-- Return the number of occurences of an element in a list
countElem :: Int -> [Int] -> Int
countElem x xs = length $ filter (x==) xs

-- Return a list with the number of occurences for the corresponding index in the sorted input list
countAll :: [Int] -> [Int]
countAll lst = [ countElem n (sort lst) | n <- [0..9]]

-- Determine if a star should be displayed if there are occurences of the element
getSym :: Int -> String
getSym x
    | x > 0 = "*"
    | otherwise = " "

-- Decrement each element of the list by one
decrementList :: [Int] -> [Int]
decrementList = map (\x -> x - 1)

-- Create a string for each line of output
-- The strings will be in reverse order to how they should be displayed
addStar :: [Int] -> [String]
addStar lst
    --Repeat until there are no more outstanding stars to add
    | all (<1) lst = [] 
    --Concatenate the individual symbols for the line, and recurse
    | otherwise = concatMap getSym lst : addStar (decrementList lst) 

-- Return the histogram for occurences in the input list
histogram :: [Int] -> String
-- Add the final messages, reverse the list of lines and concatenate them into a single string
histogram lst = unlines $ reverse ( ["0123456789"] ++ ["=========="] ++ addStar (countAll lst))
    