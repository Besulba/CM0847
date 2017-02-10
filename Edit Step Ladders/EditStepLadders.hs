module EditStepLadders where

data Tree = Tree {
            id    :: String,
            nivel :: Int,
            dad   :: String,
            tree  :: [Tree]
            }
            | Empty deriving (Show)

main :: IO ()
main = do
    contents <- getContents
    print (evalElement (map (\x -> x::String) (lines contents)) 0 [])
    --print $ map (\x -> x::String) (lines contents)

evalElement :: [String] -> Int -> [Tree] -> Int
evalElement [] n _ = n
evalElement (x:xs) n treeVal 
  | length (x:xs) > 25000 = 0
  | otherwise = if valWord (x:xs) then evalElement xs nv treeResul else 0
  where
  treeResul = insertGraph x False treeVal
  nv = getMaxNv treeResul n

valWord :: [String] -> Bool
valWord [] = True
valWord (x:xs)
  | length x >16 = False
  | otherwise = valWord xs

getMaxNv :: [Tree] -> Int -> Int
getMaxNv [] n = n
getMaxNv (Tree _ nv _ _:ts) n = getMaxNv ts maxVal
  where
  maxVal = if n>nv then n else nv

insertGraph :: String -> Bool -> [Tree] -> [Tree]
insertGraph x val []
  | val = []
  | otherwise = [treeVal]
  where 
  (treeVal,_) = insertTree x Empty 
insertGraph x val (t:ts) = treeVal: insertGraph x (val || res) ts 
  where
  (treeVal,res) = insertTree x t

insertTree :: String -> Tree -> (Tree,Bool)
insertTree x Empty = (Tree x 1 "" [],True)
insertTree x (Tree y n f [])
  | son1 && x>y = (Tree y (n+1) f [Tree x n y []],True)
  | son1 && son2 && x<=y = (Tree x (n+1) f [Tree y n x []],True)
  | otherwise = (Tree y n f [],False)
  where
  son1 = isSon x y 0
  son2 = (f == "") || isSon f x 0
insertTree x (Tree y n f (x1:xs))
  | son1 && son2 && x<=y = (Tree x (n+1) f [Tree y n x (x1:xs)],True)
  | otherwise = (Tree y newTam f listSon,nv)
  where
  son1 = isSon x y 0
  son2 = (f == "") || isSon f x 0
  (listSon,tam) = insertSon x n y False (x1:xs)
  nv = tam == n
  newTam = if nv then n else n+1
insertSon :: String -> Int -> String -> Bool -> [Tree] -> ([Tree],Int)
insertSon x tam d s []
  | son && d>x = if s then ([],tam) else ([Tree x (tam-1) d []],tam)
  | otherwise = ([],tam)
  where
  son = isSon x d 0
insertSon x tam d s (Tree y n f list:xs) -- = if state then (noAddTree,aum) else if son then (addtoTree,aum) else (noAddTree,aum)
  | state = (noAddTree, aum)
  | son = (addtoTree, aum)
  | otherwise = (noAddTree, aum)
  where
  (Tree r rt dr lr,res) = insertTree x (Tree y n f list) -- Try to insert of element in Tree
  state = s || res  -- update state if add element in Tree or no
  aum = if tam == rt then tam+1 else tam -- save the maximun nivel of the Tree
  (listTree,_) = insertSon x aum d state xs -- insert element in other Tree if is possible
  son = isSon x d 0
  noAddTree = Tree r rt dr lr:listTree -- build list Tree for return at father
  addtoTree = Tree x n f []:Tree y n f list:listTree -- add element how one Tree son of the Tree
 
isSon :: String -> String -> Int -> Bool
isSon [] [] n = n<2
isSon (_:xs) [] n = isSon xs [] (n+1)
isSon [] (_:ys) n = isSon [] ys (n+1)
isSon (x:xs) (y:ys) n
  | x == y = isSon xs ys n
  | lgx == lgy = isSon xs ys (n+1) 
  | lgx < lgy = isSon (x:xs) ys (n+1)
  | lgx > lgy = isSon xs (y:ys) (n+1)
  where
  lgx = length xs
  lgy = length ys
  