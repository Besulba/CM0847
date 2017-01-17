
-- | Little Bishop - Using Backtracking

{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where


import            Control.Lens  (element, (&), (.~))
import            Control.Monad (when, unless)
import            Data.List (intercalate)

debug ∷ Bool
debug = False

type Board    = [[Int]]
type Dim      = Int -- Dimension
type Position = (Int, Int, Dim) -- (x,y) coordinates
type Tile     = Int -- a number associate to (x,y) coordinates


data State = State 
  { board               ∷ Board
  , currentListBishops  ∷ [Tile]
  , currentPosition     ∷ Int
  , dimension           ∷ Dim
  , freeTiles           ∷ [Tile] -- tiles free from any attack
  , limitBishops        ∷ Int
  }

printBoard ∷ Board → IO ()
printBoard [] = putStrLn ""
printBoard (row:rows) = do
  putStrLn $ intercalate "\t" $ map show row
  printBoard rows

printState ∷ State → IO ()
printState state = do
  printBoard $ board state
  putStrLn ""
  putStrLn $ "freeTiles:" ++ show (freeTiles state)
  putStrLn $ "currentPosition:" ++ show (currentPosition state)
  putStrLn $ "currentListBishops:" ++ show (currentListBishops state)
  putStrLn "-------------------------------\n"


(.^) ∷ Board → ((Int, Int), Int) → Board
b .^ ((x,y), val) = b & element x . element y .~ val

getNumberPosition ∷ Position → Int
getNumberPosition (x,y,n) = n * x + y

getPosition ∷ Dim → Int → Position
getPosition n i = (i `div` n, i `mod` n, n)

getNewFreeTiles ∷ State → [Tile]
getNewFreeTiles state = do
  let n = dimension state
  let (x,y, _) = getPosition n (currentPosition state) 
  let inBoard ∷ (Int, Int, Int) → Bool
      inBoard (xx,yy,_) = and $ [0 <= xx, xx < n, 0 <= yy, yy < n ]
  let positions = [ (x + k*movx, y + k*movy, n) | (movx, movy) <- [(1,-1),(1,1),(-1,-1),(-1,1)], k <- [(-n)..n]]
  let diag = filter inBoard positions 
  let tilesNoValid = map getNumberPosition diag
  [pos | pos ← freeTiles state, pos `notElem` tilesNoValid ]


putBishop ∷ State → Int → Board
putBishop state k = do
  let b = board state
  let n = length b
  let (x,y,_) = getPosition n k
  b .^ ((x,y), k+1 ) 


-- | Number of combinations given this state

dfs ∷ State → IO Int
dfs state = do
  let goalBishop = limitBishops state 
  if goalBishop == 0 then return 1
    else do
      let n = dimension state
      let k = currentPosition state
      if k > n * n
        || null (freeTiles state)
        then return 0
        else do
          answerYes ← if k `elem` freeTiles state
              then do
                let newStateYes ∷ State
                    newStateYes = State
                      { board = putBishop state k
                      , currentListBishops = k : (currentListBishops state)
                      , currentPosition = k + 1
                      , dimension = n
                      , freeTiles = getNewFreeTiles state
                      , limitBishops = goalBishop - 1
                      }

                when debug $ printState newStateYes
                dfs newStateYes
              else return 0
                
          let newStateNo = state { currentPosition = k + 1}
          when debug $ printState newStateNo
          answerNo ← dfs newStateNo
                    
          return $ answerYes + answerNo

solve ∷ State → IO ()
solve state = do
  ans ← dfs state
  print ans

solveCase ∷ IO ()
solveCase = do
  [n,k] ∷ [Int] ← map read . words <$> getLine
  unless (n==0 && k==0) $ do
    let initState = State {
         board = [[ 0 | _ ← [1..n]] | _ ← [1..n]]
       , dimension = n
       , freeTiles = [0..(n * n -1)]
       , currentPosition = 0
       , currentListBishops = []
       , limitBishops = k
       }
    solve initState
    solveCase

main ∷ IO ()
main = solveCase
