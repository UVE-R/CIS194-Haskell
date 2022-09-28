{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Risk where

import Control.Monad.Random

import Data.List

import Control.Monad.Loops

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random :: RandomGen g => g -> (DieValue, g)
  random           = first DV . randomR (1,6)
  randomR :: RandomGen g => (DieValue, DieValue) -> g -> (DieValue, g)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

maxAttackers :: Army -> Int
maxAttackers n = min 3 (n-1)

maxDefenders :: Army -> Int
maxDefenders = min 2

diceRolls :: Int -> Rand StdGen [DieValue]
diceRolls n = replicateM n die

sortDie :: [DieValue] -> [DieValue]
sortDie = reverse . sort

matchDieRolls :: [DieValue] -> [DieValue] -> [Ordering]
matchDieRolls = zipWith compare

update :: Battlefield -> [Ordering] -> Battlefield
update (Battlefield att def) battleOutcome = Battlefield (att - defWin) (def - attWin)
                where attWin  = countOrd GT battleOutcome
                      defWin  = length battleOutcome - attWin
                      countOrd ord = length . filter (== ord)


battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield att def) = do
                attDie <- sortDie <$> (diceRolls . maxAttackers) att
                defDie <- sortDie <$> (diceRolls . maxDefenders) def
                return $ update bf $ matchDieRolls attDie defDie


invade :: Battlefield -> Rand StdGen Battlefield
invade = iterateUntilM stopBattle battle
              where stopBattle (Battlefield att def) = att < 2 || def <= 0

attWon :: Battlefield -> Bool
attWon (Battlefield att def) = def <= 0

runCnt :: Int
runCnt = 1000

successProb :: Battlefield -> Rand StdGen Double
successProb btl = do
          simBattle <- replicateM runCnt (invade btl)
          let win = length $ filter attWon simBattle
          return $ divide win runCnt
          where divide a b = fromIntegral a / fromIntegral b




          