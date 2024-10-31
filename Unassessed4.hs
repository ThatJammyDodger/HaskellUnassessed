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