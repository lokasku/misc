-- https://projecteuler.net/problem=7

import Data.List (find)

find' :: (Int -> Bool) -> [Int] -> Int
find' f (x:xs)
    | f x = x
    | otherwise = find' f xs

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = foldl (\r x -> if isMult n x then False else r) True [2..floor $ sqrt $ fromIntegral n]
    where isMult a b = (mod a b) == 0

nextPrime :: Int -> Int
nextPrime from = find' isPrime [from+1..]

doPrimes :: Int -> Int -> [Int]
doPrimes start end = np start : if (np start) < end then doPrimes (np start) end else []
    where np n = nextPrime n

-- nthPrime :: Int -> Int
-- nthPrime n = filter isPrime [2..] !! n

findPrime :: Int -> Int -> [Int]
findPrime start n = if n == 1 then [np start] else np start : findPrime (np start) (n-1)
    where np n = nextPrime n

nthPrime :: Int -> Int
nthPrime n = last (findPrime 1 n)