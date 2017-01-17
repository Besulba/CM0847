module ShellSort(sortShellSort) where
-- sort the list in the form pyramid turtle
sortShellSort :: [Int] -> [(Int,String)] -> String
sortShellSort listNum towerMapDisorder
  | validateTower listNum = ""
  | otherwise = result ++ "\n" ++ sortShellSort step towerMapDisorder
  where
  numDisorder  = findInsorted listNum
  positionNum = findPositionNumber listNum (numDisorder-1) 0
  step = findAndModifyHead listNum positionNum
  result = printSwap towerMapDisorder (head step)
-- find the element no succesor
findInsorted :: [Int] -> Int
findInsorted [] = -1
findInsorted [_] = -1
findInsorted  (l1:l2:lista)
  | l1 /= 0 = l1
  | l1 == (l2-1) = findInsorted (l2:lista)
  | otherwise = l2
-- find position of one data in the list
findPositionNumber :: [Int] -> Int -> Int -> Int
findPositionNumber [] _ _ = -1
findPositionNumber (l:list) n p
  | l==n = p
  | otherwise = findPositionNumber list n (p+1)
-- change one element at head of the list
findAndModifyHead :: [a] -> Int -> [a]
findAndModifyHead list h2 = [z]++x++y
  where
  y = drop (h2+1) list
  x = take h2 list
  z = list !! h2
-- show the person in one position in particular
printSwap :: [(Int,String)] -> Int -> String
printSwap [] _ = ""
printSwap ((key,value):s) n
  | key==n = value
  | otherwise = printSwap s n
-- validate if the tower is solve
validateTower :: [Int] -> Bool
validateTower [] = True
validateTower [_] = True
validateTower (l1:l2:list)
  | l1==(l2-1) = validateTower (l2:list)
  | otherwise = False