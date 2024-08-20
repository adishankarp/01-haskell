import Debug.Trace (trace)

digitsOfInts :: [Integer] -> [Integer]
digitsOfInts [] = []
digitsOfInts (x:xs) = digitsOfInt (x) ++ digitsOfInts(xs)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList(xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = [x] ++ [2 * y] ++ doubleEveryOther xs

listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = listReverse xs ++ [x]

digitsOfInt :: Integer -> [Integer]
digitsOfInt n 
    | n <= 0 = []
    | otherwise = digitsOfInt (n `div` 10) ++ [n `mod` 10]

-- Main function to execute the sequence of transformations
main :: IO ()
main = do   
    input <- readLn
    let m = trace ("digitsOfInt input: " ++ show (digitsOfInt input)) (digitsOfInt input)

    let p = trace ("listReverse m: " ++ show (listReverse m)) (listReverse m)

    let t = trace ("doubleEveryOther p: " ++ show (doubleEveryOther p)) (doubleEveryOther p)

    let u = trace ("digitsOfInts t: " ++ show (digitsOfInts t)) (digitsOfInts t)

    let i = trace ("sumList u: " ++ show (sumList u)) (sumList u)

    let r = trace ("i `mod` 10 == 0: " ++ show (i `mod` 10 == 0)) (i `mod` 10 == 0)
    print r
