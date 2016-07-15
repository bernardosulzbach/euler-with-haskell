import System.IO

-- We build a list with how many partitions a number N has.
-- We have a starting point which is the definition p(0) = 1.
-- Therefore, we start with the list [1,0,0,..].
-- Then for each positive integer X we do the following:
--   Take all numbers K such that K >= X
--   And add to the partition count of K the number of partitions of (K - X)
-- This counts the partitions of K by adding the partitions of all (K - X).
addInPlace src dst tab = (take dst tab) ++ [(tab !! src) + (tab !! dst)] ++ (drop (succ dst) tab)

addUntilEnd src dst max tab
    | dst == max = tab
    | otherwise = addUntilEnd (succ src) (succ dst) max (addInPlace src dst tab)

distribute cur tab = (addUntilEnd 0 cur (length tab) tab)

tableStep cur max tab
    | cur == max = tab
    | otherwise = tableStep (succ cur) max (distribute cur tab)

-- Returns the table of the partition function up to n.
-- Where the element at index i represents p(i).
buildPartitionTable n = tableStep 1 (succ n) (1 : replicate n 0)

partitions n = last (buildPartitionTable n)

-- Subtract one because the problem does not want the trivial partition.
main = do print (pred (last (buildPartitionTable 100)))
