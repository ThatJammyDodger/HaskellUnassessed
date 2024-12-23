> module UnassessedApplicatives where

defined as 

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

1. This question will have us proving some applicative laws on these two instances to improve intuition.
(a) 
We saw above that fmap f mx = pure f <*> mx. 
We know that for lists, fmap = map. 
With the definition of pure and (<*>) above, prove that pure f <*> xs is map f xs.

< pure f <*> xs = [f] <*> xs
<               = map f xs ++ [] <*> xs
<               = map f xs ++ []
<               = map f xs

---

< instance Applicative Maybe where
<   pure :: a -> Maybe a
<   pure = Just
<   
<   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
<   (<*>) Nothing _ = Nothing
<   (<*>) (Just f) mx = fmap f mx
(b) 
One of the applicative laws is as follows: mf <*> pure x = pure (\f -> f x) <*> mf. 
Its high-level intuition is saying that pures can move to the left, and don't affect the shape of mx either side it is applied.

(i) 
Using the applicative for maybes above, prove this law. 
You will need to consider cases for Nothing and Just f, but as Maybe isn't recursive, you will not require induction.

Nothing case:
< Nothing <*> pure x = Nothing <*> Just x
<               = Nothing
<               = pure (\f -> f x) <*> Nothing

Just case:
< (Just f) <*> pure x = Just f <*> Just x
<                     = Just (f x)
<                     = pure (f x)
<                     = pure (\f -> f x) <*> (Just f)

(ii) Prove mf <*> pure x = pure (\f -> f x) <*> mf for lists

mf = [] case:

< [] <*> pure x = [] <*> [x]
<               = []
<               = pure (\f -> f x) <*> []

mf = f : fs case:
IH is `fs <*> pure x = map (\f -> f x) <*> fs`

< (f : fs) <*> pure x = (f : fs) <*> [x]
<                     = map f [x] ++ (fs <*> [x])
<                     = [f x] ++ (fs <*> pure x)
<                     = [f x] ++ map (\f -> f x) <*> fs
<                     = map (\f -> f x) <*> (f : fs)
<                     = pure (\f -> f x) <*> (f : fs)
Q.E.D.


2. In the previous section, we learnt that pairs are functors. Are pairs applicatives?
(a) 
Try to give a definition of pure :: a -> (x, a), without using undefined or infinite looping.
You'll see that this function cannot be written, so pairs are not applicative.

< pure :: a -> (x, a)
< pure n = (tbc, n)

cannot find any potential 'tbc' filler due to type inconsistency therefore impossible.

(b)

> class Semigroup s where
>   (<>) :: s -> s -> s

> class Semigroup m => Monoid m where
>   mempty :: m

> instance Monoid m => Applicative ((,) m) where
>   pure :: a -> (m, a)
>   pure x = (mempty, a)
>   (<*>) :: (m, a -> b) -> (m, a) -> (m, b)
>   (<*>) (m1, f) (m2, x) = (m1 <> m2, f x)

3. (a)

Consider `sequence :: Applicative f => [f a] -> f [a]`

% Define sequence recursively, using (<$>), (<*>), and pure. 
% You will be recursive on the list structure - remember, you know nothing about the things inside other than their applicativeness.

sequence :: Applicative f => [f a] -> f [a]
