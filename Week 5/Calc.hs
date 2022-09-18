{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT 
import Parser
import StackVM
import qualified Data.Map as M

-----------------------------------------------------------------------------------

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-----------------------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-----------------------------------------------------------------------------------

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit :: Integer -> ExprT
    lit = ExprT.Lit
    add :: ExprT -> ExprT -> ExprT
    add = ExprT.Add
    mul :: ExprT -> ExprT -> ExprT
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-----------------------------------------------------------------------------------

instance Expr Integer where
    lit :: Integer -> Integer
    lit = id
    add :: Integer -> Integer -> Integer
    add = (+)
    mul :: Integer -> Integer -> Integer
    mul  = (*)

instance Expr Bool where
    lit :: Integer -> Bool
    lit  = (>0)
    add :: Bool -> Bool -> Bool
    add = (||)
    mul :: Bool -> Bool -> Bool
    mul = (&&)

newtype MinMax = MinMax Integer
    deriving (Eq, Show)

instance Expr MinMax where
    lit :: Integer -> MinMax
    lit = MinMax
    add :: MinMax -> MinMax -> MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul :: MinMax -> MinMax -> MinMax
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)

instance Expr Mod7 where
    lit :: Integer -> Mod7
    lit x = Mod7 $ mod x 7
    add :: Mod7 -> Mod7 -> Mod7
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x + y) 7
    mul :: Mod7 -> Mod7 -> Mod7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x * y) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer
testBool :: Maybe Bool
testMM :: Maybe MinMax
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-----------------------------------------------------------------------------------

instance Expr Program where
    lit :: Integer -> Program
    lit a = [StackVM.PushI a]
    add :: Program -> Program -> Program
    add a b = a ++ b ++ [StackVM.Add]
    mul :: Program -> Program -> Program
    mul a b = a ++ b ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul

-----------------------------------------------------------------------------------

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT
                | Var String
            deriving (Show, Eq)

instance Expr VarExprT where
    lit :: Integer -> VarExprT
    lit = Calc.Lit
    add :: VarExprT -> VarExprT -> VarExprT
    add = Calc.Add
    mul :: VarExprT -> VarExprT -> VarExprT
    mul = Calc.Mul

instance HasVars VarExprT where
    var :: String -> VarExprT
    var = Calc.Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var :: String -> M.Map String Integer -> Maybe Integer
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit :: Integer -> (M.Map String Integer -> Maybe Integer)
    lit a _ = Just a

    add :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer)
    add a b m = (+) <$> a m <*> b m

    mul :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer)
    mul a b m = (*) <$> a m <*> b m
    
withVars :: [(String, Integer)]-> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs