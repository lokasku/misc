-- https://projecteuler.net/problem=10

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = foldl (\r x -> if isMult n x then False else r) True [2..floor $ sqrt $ fromIntegral n]
    where isMult a b = (mod a b) == 0

sumPrime :: Int -> Int
sumPrime limit = sum [p | p <- [2..limit], isPrime p]