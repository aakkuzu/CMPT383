-- Anastasia Akkuzu
-- 301316633

det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt (det a b c))/2*a
quadsol2 a b c = (-b + sqrt (det a b c))/2*a

third_a (x:xs) = xs !! 1
third_b (a:b:c:_) = show c

fact 0 = 1
fact x = x * fact (x-1)

hailstone n
    | even n = div n 2
    | otherwise = 3 * n + 1

hailLen n
    | n==1 = 0
    | otherwise = 1 + hailLen (hailstone (n))
