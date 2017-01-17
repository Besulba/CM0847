--p multiplied by 2-9 while p is less than n
import Control.Monad 
import System.IO

--analize the next turn for evaluate the best game
game :: Int -> Int -> Bool -> String
game n p men 
  | p<n = if men then game n (p*9) (not men) else game n (p*2) (not men)
  | otherwise = if men then "Ollie wins." else  "Stan wins."

--init game if n is in the range else finish game
multiplicationGame :: Int -> String
multiplicationGame n
   | 1<n && n<4294967295 = game n 1 True 
   | otherwise = "Number is bad"
--get list with each element of the file
main :: IO ()
main = do
    end <- isEOF
    unless end $ do
        n <- getLine
        putStrLn $  multiplicationGame ( read n :: Int )
        main