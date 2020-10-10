--Anastasia Akkuzu
--301316633

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
--import Data.Time.Calendar.Week



merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys)
    | x < y = x : merge (xs) (y:ys)
    | otherwise = y : merge (x:xs) (ys)


mergeSplit :: [a] -> ([a], [a])
mergeSplit x = (take n x, drop n x)
    where n = (length x) `div` 2 


mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs
    | (length xs) > 1 = merge (mergeSort ls) (mergeSort rs)
    | otherwise = xs
    where (ls, rs) = mergeSplit xs


daysInYear :: Integer -> [Day]
daysInYear y = [jan1 .. dec31]
    where jan1 = fromGregorian y 1 1
          dec31 = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday x
    | snd (sundayStartWeek x) == 5 = True
    | otherwise = False

getDay (y,m,d) = d

divisors :: Int -> [Int]
divisors n = [ i | i <- [2..(n `div` 2)], n `mod` i == 0]

isPrimeDay :: Day -> Bool
isPrimeDay d
    | divisors ( getDay ( toGregorian d)) == [] = True
    | otherwise = False


primeFriday :: Integer -> [Day]
primeFriday y = filter isPrimeDay( filter isFriday (daysInYear y))

