module Unassessed4 where

import Data.Char
import Data.List


--1.

-- Rewrite using either map, concatMap, filter or some kind of fold:

-- depunctuate :: [Char] -> [Char]
-- depunctuate = foldr (\x acc -> if elem x ['.', ',', ':'] then acc else x : acc) [] 

depunctuate :: [Char] -> [Char]
depunctuate = filter (\x -> x `notElem` ['.', ',', ':'])

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
facts = scanl' (*) 1 [1..]

eApprox :: Int -> Double
eApprox n = sum $ take n (map (1 /) facts)

mystery = 1 : scanl (+) 1 mystery -- FIBONACCI

-- 4.
squash :: (a -> a -> b) -> [a] -> [b]

-- squash _ [] = []
-- squash f [x] = []
-- squash f (x : x' : xs) = f x x' : squash f (x' : xs)

squash f l = zipWith f (tail l) l


-- 5. 
converge :: forall a. (a -> a -> Bool) -> [a] -> a
converge f xs = go xs (tail xs)
  where 
    go :: [a] -> [a] -> a
    go [x] [] = x
    go (y : ys) (z : zs)
      | f y z     = y
      | otherwise = go ys zs

e :: Double
-- e = converge (==) (scanl (\y x -> y + recip x) 0 (scanl (*) 1 [1..]))
-- point free using pointfree.io
e = converge (==) (scanl ((. recip) . (+)) 0 (scanl (*) 1 [1..]))


-- 7.
-- any :: (a -> Bool) -> [a] -> Bool
-- any f = or . map f
all :: (a -> Bool) -> [a] -> Bool
all f = and . map f


-- 8.
isElem :: Eq a => a -> [a] -> Bool
isElem = any . (==)


-- 9b
(<.>) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(<.>) = (.) . (.)