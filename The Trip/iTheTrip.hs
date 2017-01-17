import TheTrip

-------------------- Get List File -------------------------
--delete lenght element of the list
partition :: [Double] -> [[Double]]
partition [] = []
partition (x:xs) = w : partition (drop xint xs)
    where
        xint = round x::Int
        w = take xint xs
--call the Trip with each list
process :: [Double] -> IO()
process xs = let res = partition xs in mapM_ theTrip res
--get list with each element of the file       
main :: IO ()
main = do
    contents <- getContents
    process  $ map (\x -> read x::Double) (lines contents)