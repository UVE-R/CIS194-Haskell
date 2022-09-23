{-# LANGUAGE InstanceSigs #-}
module Party where

import Employee
import Data.Tree
import Data.List


-- Add employee to GuestList
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)

-- Monoid instance for GuestList
instance Monoid GuestList where
    mempty :: GuestList
    mempty = GL [] 0

instance Semigroup GuestList where
    (<>) :: GuestList -> GuestList -> GuestList
    (<>) (GL xs fun1) (GL ys fun2) = GL (xs ++ ys) (fun1 + fun2)


-- Return GuestList with maximum fun
-- Ord for GuestList is defined in Employee.hs
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

----------------------------------------------------------------------

-- Fold for trees
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node lab sf) = f lab (map (treeFold f) sf)

----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp guests = (glCons emp gl1, gl2)
    where gl1 = foldMap snd guests
          gl2 = foldMap fst guests

----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

----------------------------------------------------------------------

genMessage :: GuestList -> String
genMessage (GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++ names
    where names = unlines $ sort $ map empName emps

main :: IO()
main = do
    compHeirarchy <- readFile "company.txt"
    putStrLn $ genMessage $ maxFun $ read compHeirarchy

