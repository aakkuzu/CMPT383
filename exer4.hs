-- Anastasia Akkuzu
--301316633

fact 0 = 1
fact x = x * fact (x-1)

choose :: Int -> Int -> Int
choose n k = (fact n) `div` (fact k * fact (n-k))

pascal :: Int -> [Int]
pascal n = [choose n k | k <- [0..n]] 


addPair :: (Int, Int) -> Int
addPair = uncurry (+)


withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter (/=0)

searchFor :: Eq t => t -> [(t, a)] -> Maybe a
searchFor _ [] = Nothing
searchFor x ((a,b):xs)
    |(x == a) = Just b
    |otherwise = searchFor x xs

findElt :: (Eq t, Num a, Enum a) => t -> [t] -> Maybe a
findElt a (x) = searchFor a (zip (x) [0..])


