-- https://projecteuler.net/problem=1

getMultiple :: Int -> Int -> Int
getMultiple a b = sum [x | x <- [1..999], mod x a == 0 || mod x b == 0]

sumMultipleOfThreeOrFive = getMultiple 3 5