-- https://projecteuler.net/problem=4

palindromeProduct1 :: Int -> [Int]
palindromeProduct1 limit =
    foldr f [] [100..limit]
    where f x r = foldr (g x) [] [100..limit] ++ r
          g x y r =
            if read (reverse $ show (x*y)) == x*y
            then (x*y) : r
            else r

palindromeProduct2 :: Int -> [Int]
palindromeProduct2 limit =
    concatMap (\x -> filter isPalindrome $ map (x*) [100..limit]) [100..limit]
    where isPalindrome xy = read (reverse $ show xy) == xy

palindromeProduct3 :: Int -> [[Int]]
palindromeProduct3 limit =
    [[xy | y <- [100..limit], let xy = x*y, isPalindrome xy] | x <- [100..limit]]
    where isPalindrome xy = read (reverse $ show xy) == xy

higherPalindrome1 = maximum $ palindromeProduct1 1000
higherPalindrome2 = maximum $ palindromeProduct2 1000
higherPalindrome3 = maximum $ palindromeProduct3 1000