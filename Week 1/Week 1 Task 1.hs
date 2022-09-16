{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

-- convert postive intger to a list of digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise =  toDigits (div n 10) ++ [mod n 10]

-- convert positive intger to a list of digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- double every other integer from the start of the list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther listIn = reverse $ doubleEveryOtherHelper $ reverse listIn

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:xs) =
    if null xs then [x] -- special case for only 1 item
    else  x : head xs * 2 : doubleEveryOtherHelper (tail xs)

-- sum each digits of each integer in the list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + sumDigits xs

-- determines if the card number is valid
validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0