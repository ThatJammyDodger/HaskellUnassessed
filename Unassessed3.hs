--This module is for the 3rd set of unassessed exercises.
module Unassessed3 where
import Data.List ( (\\) )
import Data.Char
import Distribution.Simple.PackageIndex (allPackages)

-- One
-- (a) No because the thing added in front is a String not a Char
-- (b) "Ongoing"
-- (c) "Lugworm"
-- (d) [] WAIT WHAT!? Why does it output "" instead of []???!!!!
-- (e) 1. NO. We are here examining the length of a list of lists
-- etc etc, cannot be bothered to type all these


-- 2.
precedes :: String -> String -> Bool
precedes [] ys = True
precedes xs [] = False
precedes (x : xs) (y : ys)
 | x == y    = precedes xs ys
 | otherwise = x < y

--3.
pos :: Int -> [Int] -> Int
pos x ns = posHelper x ns 0

posHelper :: Int -> [Int] -> Int -> Int
posHelper _ [] _ = -1
posHelper x (n : ns) i
  | x == n    = i
  | otherwise = posHelper x ns (i + 1)

post :: Char -> [Char] -> Int
post x xs = pos (ord x) ([ord x | x <- xs])

-- 4. 
twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (x : xs) = elem x xs || twoSame xs




-- 5.
rev :: [a] -> [a]
rev xs = revHelper xs []
  where 
    revHelper :: [a] -> [a] -> [a]
    revHelper []       reversed = reversed
    revHelper (x : xs) reversed = revHelper xs (x : reversed)

-- 6.
substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring (s' : s) (m' : m)
  | s' == m'  = substring s m
  | otherwise = substring (s' : s) m

-- 11.
primeFactors :: Int -> [Int]
primeFactors n = facts
  where (facts, _) = primeFactorsHelper n 2 []

--                     n     checkN   ex.facts   facts  checkNum
primeFactorsHelper :: Int -> Int -> [ Int ] -> ([ Int ], Int)
primeFactorsHelper 1 _ facts = (facts, 0)
primeFactorsHelper n check facts
  | n `mod` check == 0 = primeFactorsHelper (n `div` check) check (facts ++ [check])
  | otherwise = primeFactorsHelper n (check + 1) facts


-- PART 2 LIST COMPREHENSIONS

{-
Tony Hoare’s famous “quicksort” algorithm works by partitioning a (non-empty) list into those elements 
less than or equal to that at the head and those greater than that at the head. These two lists are then 
recursively sorted and the results appended together, with the head element sandwiched between the two. 
Define a Haskell version of quicksort that generates the two sublists using list comprehensions.
-}

-- 2. Quicksort
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = (quicksort [a | a <- xs, a <= x]) 
                     ++ [ x ] 
                     ++ quicksort ([a | a <- xs, a > x])



-- 4.
prefixes :: String -> [String]
prefixes [c] = [[c]]
prefixes c = [ take n c | n <- [1.. length c]]

-- 5.
substrings :: String -> [String]
substrings [] = []
substrings (s : ss) = prefixes (s : ss) ++ substrings ss

-- 6.
perms :: Eq a => [a] -> [[a]]
perms []  = [[]]
perms xs = concat [ [ y : zs | zs <- perms (xs \\ [y])] | y <- xs ]


-----------------------------------------------------------

--7.
{-|
Suppose a graph is defined by a list of its edges, where each edge is specified as a pair of node identifiers. For example, if the node identifiers are (unique) integers, the following represents a circular graph with four nodes: [(1,2), (2,3), (3,4), (4,1)]. Define a function routes :: Int-> Int-> [(Int, Int)]-> [[Int]] that will compute all routes from a specified node to another in a given acyclic graph, i.e. a graph with no cycles. For example, routes 1 6 [(1,2), (1,3), (2,4), (3,5), (5,6), (3,6)] should return [[1,3,5,6], [1,3,6]].
|-}

--             start   end      edges         all (x,z)s
routesFilter :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
routesFilter x y edges = [ (x, z) | (a, z) <- edges, a == x ]

--       start   end      edges
routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes s e edges
  | s == e    = [[s]]
  | otherwise = [ s : route | from' <- [ en | (st, en) <- edges, st == s ], route <- routes from' e edges ]



-- stupid function since `elem` already exists
-- isInList :: Eq a => a -> [a] -> Bool
-- isInList _ []       = False
-- isInList x (y : ys) = (x == y) || isInList x ys