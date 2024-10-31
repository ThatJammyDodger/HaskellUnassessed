module Unassessed2 where
import GHC.Integer (integerToInt)

addDigit :: Int -> Int -> Int
addDigit a b = 10 * a + b

toFahrenheit :: Float -> Float
toFahrenheit c = c * 9 / 5 - 32

type Vertex = (Float, Float)

distance :: Vertex -> Vertex -> Float
distance (a, a') (b, b') = sqrt ((a - b) ^ 2 + (a' - b') ^ 2)

triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea v1 v2 v3 = sqrt (s * (s - a) * (s - b) * (s - c))
                        where s = (a + b + c) / 2
                              a = distance v1 v2
                              b = distance v2 v3
                              c = distance v3 v1

turns :: Float -> Float -> Float -> Float
turns start end r = 2 * pi * r / distance
                    where distance = end - start


-- isPrime :: Int -> Bool
-- isPrime n = null [ x | x <- [2, 3..  floor (sqrt (fromIntegral n))], (n `mod` x) == 0 ]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrimeHelper n 2

isPrimeHelper :: Int -> Int -> Bool
isPrimeHelper n i = (i * i > n) || (n `mod` i /= 0) && isPrimeHelper n (i + 1)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- obviously this function requires n >= r >= 0
perm :: Int -> Int -> Int
perm n 0 = 1
perm n r = (n - r + 1) * perm n (r - 1)

-- combinatorically, we know that nCr = (n-1)Cr + (n-1)C(r-1)
choose :: Int -> Int -> Int
choose n 0 = 1
choose n r
  | n == r = 1
choose n r = choose (n - 1) r + choose (n - 1) (r - 1)

-- a % b
remainder :: Int -> Int -> Int
remainder a b
  | a < b     = a
  | otherwise = remainder (a - b) b

-- a // b
quotient :: Int -> Int -> Int
quotient a b
  | a < b     = 0
  | otherwise = 1 + quotient (a - b) b

binary :: Int -> Int
binary n
  | n < 2     = n
  | otherwise = 10 * binary (quotient n 2) + remainder n 2


-- FROM COPILOT AS I WANTED TO JUST SKIP TO THE LAST QUESTION
fibh :: Int -> Int -> Int -> Int -> Int
fibh k fk fk1 0 = fk
fibh k fk fk1 n = fibh (k + 1) fk1 (fk + fk1) (n - 1)

fib :: Int -> Int
fib n = fibh 0 1 1 n
-------------------------------------------------------------

basicRatio :: Int -> Int -> Float
basicRatio nSub n = fromIntegral (fib n) / fromIntegral (fib nSub)

goldenRatio :: Float -> Float
goldenRatio e = goldenRatioHelper e (basicRatio 0 1) 0 1
  where
    --                   e        r(k-1)   f(k-1)   f(k)     r(k)
    goldenRatioHelper :: Float -> Float -> Float -> Float -> Float
    goldenRatioHelper e rKsub fKsub fK
      | abs (rK - rKsub) < e = rK
      | otherwise            = goldenRatioHelper e rK fK (fK + fKsub)
      where rK = fK / fKsub
