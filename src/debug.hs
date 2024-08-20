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



main :: IO ()
main = do   
    input <- readLn
    let m = digitsOfInt input

    putStrLn "Hello"
    print $ input
    print $ m

    let p = listReverse m
    print $ p

    let t = doubleEveryOther p
    print $ t

    let u = digitsOfInts t
    print $ u

    let i = sumList u
    print $ i

    let r = i `mod` 10 == 0
    print $ r

     
