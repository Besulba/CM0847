import ShellSort
-- return key of the list ordered
mapList :: [(a, t)] -> [a]
mapList = map fst --get the first element
-- add key in disorder of the one list
getMapListDisorder :: Eq a => [(t, a)] -> [a] -> [(t, a)]
getMapListDisorder [] _ = []
getMapListDisorder _ [] = []
getMapListDisorder ((key,val):xs) (y:ys)
  | val == y = (key,y): getMapListDisorder xs ys
  | otherwise = getMapListDisorder (xs++[(key,val)]) (y:ys)
-- add key succesor at value of the second list
getMapListOrder :: Num t1 => t1 -> [t] -> [(t1, t)]
getMapListOrder _ [] = []
getMapListOrder key (x:xs) = (key,x): getMapListOrder (key+1) xs
-- get lines of the list turtle
getTower :: Int -> IO [String]
getTower 0 = return []
getTower h = do
  turtle <- getLine
  resto <- getTower (h-1)
  return $ turtle : resto
-- read each a of the case of the file
readCase :: Int -> String -> IO ()
readCase 0 result = putStrLn result
readCase n result = do
  m <- getLine
  let h = read m :: Int
  towerDisorder <- getTower h
  towerSorter <- getTower h
  let towerMapSort = getMapListOrder 0 towerSorter
  let towerMapDisorder = getMapListDisorder towerMapSort towerDisorder
  let listKey = mapList towerMapDisorder
  let stepResult = sortShellSort listKey towerMapDisorder
  readCase (n-1) (result++stepResult++"\n")
--get list with each element of the file
main :: IO ()
main = do
    n <- getLine
    let numCases = read n :: Int
    readCase numCases ""