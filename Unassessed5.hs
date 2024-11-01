module Unassessed5 where

-- 1.

type Vertex = (Float, Float)
data Shape = Triangle Float Float Float
             | Square Float
             | Circle Float
             | Convex [Vertex]

-- and  2.

area :: Shape -> Float
area (Square x) = x * x
area (Circle r) = pi * r * r
area (Triangle a b c) = 
    let s = (a + b + c) / 2
    in  sqrt (s * (s - a) * (s - b) * (s - c))
area (Convex [])     = 0
area (Convex [_])    = 0
area (Convex [_, _]) = 0
area (Convex (x1 : x2 : x3 : xs)) =
  area (Triangle 
          (side x1 x2)
          (side x2 x3)
          (side x3 x1)) +
  area (Convex (x1 : x3 : xs))
  where
    side :: Vertex -> Vertex -> Float
    side (x,y) (x',y') =
        sqrt ((x - x') ^ 2 + (y - y') ^ 2)

-- 3.
data Date = Date Int Int Int
-- no, too long and not necessarily hard

-- 4.
data Tree = Leaf | Node Tree Tree deriving (Eq, Show)

makeTrees :: Int -> [Tree]
makeTrees 0 = [Leaf]
makeTrees n = [Node subtree Leaf, Node Leaf subtree | subtree <- makeTrees (n - 1)]