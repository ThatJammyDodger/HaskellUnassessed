module Unassessed4 where

import Data.Char


--1.

-- Rewrite using either map, concatMap, filter or some kind of fold:

-- depunctuate :: [Char] -> [Char]
-- depunctuate = foldr (\x acc -> if elem x ['.', ',', ':'] then acc else x : acc) [] 

depunctuate :: [Char] -> [Char]
depunctuate = filter (\x -> not (elem x ['.', ',', ':']))

makeString :: [Int] -> [Char]
makeString = map chr

enpower :: [Int] -> Int
enpower = foldr1 (flip (^))

-- revAll :: [[a]] -> [a]
-- revAll = foldr (\x acc -> reverse x ++ acc) []

revAll :: [[a]] -> [a]
revAll = concatMap reverse

-- rev :: [a] -> [a]
-- rev = foldr (\x acc -> acc ++ [x]) []

-- better:
rev :: [a] -> [a]
rev = foldl f []
  where
    f xs y = y : xs


dezip :: [(a,b)] -> ([a],[b])
dezip = foldr f ([], [])
  where
    f (x, y) (l1, l2) = (x : l1, y : l2)

-- 2.

allSame :: [Int] -> Bool
-- allSame []            = True
-- allSame (x : [])      = True
-- allSame (x : x' : xs) = x == x' && allSame (x' : xs)

allSame list = and (zipWith (==) list (tail list))

-- 3.

facts :: [Double]
facts = scanl (*) 1 [1..]

e_approx :: Int -> Double
e_approx n = sum $ take n (map ((/) (fromIntegral 1)) facts)

mystery = 1 : scanl (+) 1 mystery -- FIBONACCI

-- 4.
squash :: (a -> a -> b) -> [a] -> [b]

-- squash _ [] = []
-- squash f [x] = []
-- squash f (x : x' : xs) = f x x' : squash f (x' : xs)

squash f l = zipWith f (tail l) l


-- 5. 