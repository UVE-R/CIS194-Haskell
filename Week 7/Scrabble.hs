{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty  = Score 0


score :: Char -> Score
score x
    | check "aeilnorstu" = Score 1
    | check "dg" = Score 2
    | check "bcmp" = Score 3
    | check "fhvwy" = Score 4
    | check "k" = Score 5
    | check "jx" = Score 8
    | check "zq" = Score 10
    | otherwise = Score 0
    where c = toLower x
          check = elem c

scoreString :: String -> Score
--scoreString = foldr ((<>) . score) (Score 0)
scoreString = mconcat . fmap score