import JollyJumpers(jollyJumpers)
import Data.List.Utils

{-
For the execution of the next program you need the Data.List.Utils library
in consequece you need cabal install missingh
-}

--get list with each element of the file       
main :: IO ()
main = do
    contents <- getLine
    print (jollyJumpers $ map (\x -> read x::Int) (split " " contents))
    main