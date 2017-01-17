module JollyJumpers (jollyJumpers) where

--validate if one list is Jolly or No Jolly
jollyJumpers :: [Int] -> String
jollyJumpers (n:list) = if listlength && validateJolly (n-1) listSort then "Jolly" else "No Jolly"
  where
  listSec = secuenceJolly list
  listSort = sort listSec
  listlength = n-1 == length listSort
jollyJumpers _ = "Jolly"
--get one list Jolly and the max value of the list
secuenceJolly :: [Int] -> [Int]
secuenceJolly [] = []
secuenceJolly [_] = []
secuenceJolly (x1:x2:s) = abs(x1-x2) : list
  where
  list = secuenceJolly (x2:s)
--return true if the list is Jolly for the max in otherwise false
validateJolly :: Int -> [Int] -> Bool
validateJolly n [] = n==0
validateJolly n [x1] = n==x1
validateJolly n (x1:s)
  | n == x1 = validateJolly (n-1) s
  | otherwise = False
--Order list and delete element duplicates
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = put x (sort xs)

put :: Ord a => a -> [a] -> [a]
put y [] = [y]
put y (x:xs)
    | y > x = y:x:xs
    | y == x = y:xs
    | otherwise = x:put y xs