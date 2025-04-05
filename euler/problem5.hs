--  https://projecteuler.net/problem=5 

find' :: (Int -> Bool) -> [Int] -> Int
find' f (x:xs)
    | f x = x
    | otherwise = find' f xs

smallestMultiple :: Int -> Int
smallestMultiple n = find' (\x -> (all (\y -> (mod x y) == 0) [1..n])) [1..]