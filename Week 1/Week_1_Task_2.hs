type Peg = String
type Move = (Peg, Peg)

-- move n disks from 1st to 2nd peg, using the 3rd as a temporary peg
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
-- move n-1 disks from p1 to temp, using p2 as the new temporary peg
-- move the disk remaining from p1 to p2
-- move n-1 disks from temp to p2, using p1 as the new temporary peg
hanoi3 n p1 p2 temp 
    | n <= 0 = [] 
    | n == 1 = [(p1, p2)] -- base case: move disk from start to goal
    -- move n-1 disks from p1 to temp, using p2 as the new temporary peg
    -- move the disk remaining from p1 to p2
    -- move n-1 disks from temp to p2, using p1 as the new temporary peg
    | otherwise = hanoi3 (n - 1) p1 temp p2 ++ hanoi3 1 p1 p2 temp  ++ hanoi3 (n - 1) temp p2 p1 


-- number of disks which will be left on the peg being moved from
k :: Integer
k = 3
-- move n disks from 1st to 4th peg, using the 2nd and 3rd as temporary pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n p1 temp1 temp2 p2 
    | n <= 0 = []
    | n == 1 = [(p1, p2)] -- base case: move disks from start to goal
    | n <= 3 = hanoi3 n p1 p2 temp1 -- with 2 or 3 disks, there is no improvement with 4 pegs
    -- move n-k disks from p1 to temp2, temp1 and p2 can be used as auxillary
    -- move k disks from p1 to p2, only temp1 can be used as auxillary as temp2 has smaller disks on it
    -- move n-k disks from temp2 to p2, p1 and temp1 can be used as auxillary
    | otherwise = hanoi4 (n-k) p1 temp1 p2 temp2 ++ hanoi3 k p1 p2 temp1 ++ hanoi4 (n-k) temp2 p1 temp1 p2 

    