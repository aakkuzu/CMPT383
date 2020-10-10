--Anastasia Akkuzu
--301316633


divisors :: Int -> [Int]
divisors n = [ i | i <- [2..(n `div` 2)], n `mod` i == 0]
primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]


pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [b..c], (a*a) + (b*b) == (c*c)]


join :: [a] -> [[a]] -> [a]
join a [] = []
join a [xs] = xs
join a (x:xs) = x ++ a ++ (join a xs)


--fact 0 = 1
--fact x = x * fact (x-1)
--multiply the list of numbers all together until N
fact' n = foldl (*) 1 [1..n]


hailstone n
    | even n = div n 2
    | otherwise = 3 * n + 1

hailLen n = hailTail 0 n
    where
        hailTail a 1 = a
        hailTail a n = hailTail (a+1) (hailstone (n))

