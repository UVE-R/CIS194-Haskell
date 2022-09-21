{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- get annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Append two join-lists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-----------------------------------------------------------------------------

-- Return the size for a given JoinList
getSizeTag :: (Monoid b, Sized b) => JoinList b a -> Int
getSizeTag = getSize . size . tag

-- Return the annotation at the index specified in the JoinList
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing

-- Check if index is in range
indexJ i _ | i < 0 = Nothing
indexJ i x | i >= getSizeTag x = Nothing

indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ n t@(Append m left right)
    | getSizeTag left > n = indexJ n left --Check if it is possible to go left
    | otherwise = indexJ (n - getSizeTag left) right --Update index
    
-----------------------------------------------------------------------------

-- drop first n elements from a JoinList
dropJ :: (Sized b, Monoid b) =>Int -> JoinList b a -> JoinList b a

dropJ _ Empty = Empty
dropJ n x 
    | n <= 0 = x -- If n is <=0 then there are no more elements to drop
    | n > getSizeTag x = Empty -- If n if greater then the 
dropJ _ (Single _ _) = Empty

dropJ n (Append m left right)
    --If the number of elements on the left JoinList is less than n, they will all be dropped, leaving to dropping from the right JoinList
    | getSizeTag left < n = dropJ (n - getSizeTag left) right 
    --If the number of elements on the left JoinList is more than or equal to n, only some will be dropped and none from the right JoinList
    | otherwise = dropJ n left +++ right

-----------------------------------------------------------------------------

-- take the first n elements from a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a

takeJ _ Empty = Empty
 
-- Check bounds
takeJ n x 
    | n <= 0 = Empty
    | n >= getSizeTag x = x

takeJ _ x@(Single _ _) = x

takeJ n (Append m left right)
    --If the number of elements on the left JoinList is less than n, they will all be taken, leaving to taking from the right JoinList
    | getSizeTag left < n = left +++ takeJ (n - getSizeTag left) right 
    --If the number of elements on the left JoinList is more than or equal to n, none will need to be taken from the right JoinList
    | otherwise = takeJ n left


scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-----------------------------------------------------------------------------


instance Monoid m => Semigroup (JoinList m a) where
  (<>) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
  (<>) = (+++)

instance Monoid m => Monoid (JoinList m a) where
    mempty :: Monoid m => JoinList m a
    mempty = Empty

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


instance Buffer (JoinList (Score, Size) String) where
    toString :: JoinList (Score, Size) String -> String
    toString = unlines . jlToList

    fromString :: String -> JoinList (Score, Size) String
    fromString = mconcat . fmap (\x -> Single (scoreString x, Size 1) x ) . lines
    
    line :: Int -> JoinList (Score, Size) String -> Maybe String
    line = indexJ

    replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
    replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl

    numLines :: JoinList (Score, Size) String -> Int
    numLines = getSizeTag

    value :: JoinList (Score, Size) String -> Int
    value = getScore . fst . tag
        where getScore (Score i) = i



