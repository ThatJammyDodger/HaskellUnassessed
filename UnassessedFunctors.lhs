> module UnassessedFunctors where

1. Functors

two laws: identity (fmap id = id), composition (fmap f . fmap g = fmap (f . g))

This first question explores
data Maybe a = Just a | Nothing
as a functor.

(a)

> maybeMap :: (a -> b) -> Maybe a -> Maybe b
> maybeMap f (Just x) = Just (f x)
> maybeMap _ _        = Nothing

(b) proofs:

< maybeMap id (Just x) = Just (id x)
<                      = Just x
<                      = id (Just x)
< maybeMap id Nothing = Nothing
<                     = id (Nothing)
< maybeMap f (maybeMap g Nothing) = maybeMap f Nothing
<                                 = Nothing
<                                 = maybeMap (f . g) Nothing
< maybeMap f (maybeMap g (Just x)) = maybeMap f (Just (g x))
<                                  = Just (f (g x))
<                                  = Just ((f . g) x)
<                                  = maybeMap (f . g) (Just x)

(c) checking something else

functor laws require:
< oddMap id (Just x) = id (Just x)
but this thing has
< oddMap id (Just x) = Nothing
% ###


2.

< map :: (a -> b) -> [a] -> [b]
< map _ [] = []
< map f (x : xs) = f x : map f xs

proving composition law for map:

map f (map g xs)

Spse xs = []  {base case}

< map f (map g [])
<   = % by def of map
< map f []
<   = % again
< []
<   = % by def of map but in reverse.
< map (f . g) []

Inductive hypothesis is then
< map f (map g xs) = map (f . g) xs

Proving recursive case:
< map f (map g (x : xs)) = map f (g x : map g xs)
<                        = f (g x) : map f (map g xs)
<                        = f (g x) : map (f . g) xs
<                        = (f . g) x : map (f . g) xs
<                        = map (f . g) (x : xs)
Q.E.D.

3. Tuples are also functors and the map works for their last parameter.
(a) def

> pairMap :: (a -> b) -> (x, a) -> (x, b)
> pairMap f (x', x'') = (x', f x'')

(b) proof

identity law:

< pairMap id (x, y) = (x, id y)
<                   = (x, y)
<                   = id (x, y)

composition law

< pairMap f (pairMap g (x, y)) = pairMap f (x, g y)
<                              = (x, f (g y))
<                              = (x, (f . g) y)
<                              = pairMap (f . g) (x, y)

4. defining an actual functor instance

(a) completing an instance

> data Treee a = Leaf a | Sprout a a | Fork (Treee a) a (Treee a)

> instance Functor Treee where
>   fmap :: (a -> b) -> Treee a -> Treee b
>   fmap f (Leaf x) = Leaf (f x)
>   fmap f (Sprout x x') = Sprout (f x) (f x')
>   fmap f (Fork x x' x'') = Fork (fmap f x) (f x') (fmap f x'')

(b) proving the functor laws on this instance

identity inductively with IH of `fmap id x = id x` where x is a Fork
base cases:
< fmap id (Leaf x) = Leaf (id x)
<                  = Leaf x
<                  = id (Leaf x)
< fmap id (Sprout x x') = Sprout (id x) (id x')
<                       = Sprout x x'
<                       = id (Sprout x x')

recursive case using IH:
< fmap id (Fork x x' x'') = Fork (fmap id x) (id x') (fmap id x'')
<                         = Fork (id x) (x') (id x'')
<                         = Fork x x' x''
<                         = id (Fork x x' x'')
Q.E.D.

function composition with IH of `fmap f (fmap g x) = fmap (f . g) x` where x is a Fork
base cases:
< fmap f (fmap g (Leaf x)) = fmap f (Leaf (g x))
<                          = Leaf (f (g x))
<                          = Leaf ((f . g) x)
<                          = fmap (f . g) (Leaf x)
< fmap f (fmap g (Sprout x x')) = fmap f (Sprout (g x) (g x'))
<                               = Sprout (f (g x)) (f (g x'))
<                               = Sprout ((f . g) x) ((f . g) x')
<                               = fmap (f . g) (Sprout x x')

recursive case using IH:
< fmap f (fmap g (Fork x x' x'')) = fmap f (Fork (fmap g x) (g x') (fmap g x''))
<                                 = Fork (fmap f (fmap g x)) (f (g x')) (fmap f (fmap g x''))
<                                 = Fork (fmap (f . g) x) ((f . g) x') (fmap (f . g) x'')
<                                 = fmap (f . g) (Fork x x' x'')

