module TheTrip(theTrip) where

--return a line stating the total amount of money, in dollars and cents, that must be exchanged to equalize the students' costs.
theTrip :: [Double] -> IO()
theTrip [] = return ()
theTrip cost 
        | (length cost < 1001) && (maximum cost < 10000.00) = print (fromIntegral result / 100)
        | otherwise = print "Hello, something went wrong"
    where
    costInt = mult 100 cost
    med = round (half cost*100) :: Int
    result = balanceMax med costInt
--subtracts 1 from the maximum value from the list
balanceMax :: Int -> [Int] -> Int
balanceMax med nCost = if maximum nCost > med+1 then 1 + balanceMin med eqCost else 1
    where
    eqCost=addValue (maximum nCost) (-1) nCost
--adds 1 to the minimum value of the list
balanceMin :: Int -> [Int] -> Int
balanceMin med nCost = if minimum nCost < med-1 then balanceMax med eqCost else 1
    where
    eqCost=addValue (minimum nCost) 1 nCost
--find an item in a list and adds y + z 
addValue :: Int -> Int -> [Int] -> [Int]                         
addValue _ _ [] = []
addValue y z (x:xs)
  | x==y           = (y+z):xs
  | otherwise      = x:addValue y z xs
--multiply a list by a value n
mult :: Int -> [Double] -> [Int]
mult _ [] = []
mult m (x:xs) = ((round x :: Int)*m) : mult m xs
--obtains the average
half :: [Double] -> Double
half nSum = sum nSum / fromIntegral (length nSum)