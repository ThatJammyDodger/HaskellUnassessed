> module UnassessedMonads where

> import Prelude hiding ()

1. Earlier, we met Monoid m => Applicative ((,) m). It turns out that this is indeed also a monad.

(a)

< instance Monoid m => Monad ((,) m) where
<   (>>=) :: (m, a) -> (a -> (m, b)) -> (m, b)
<   (>>=) (mx, ma) mf = (mx <> m1, m2)
<     where (m1, m2) = mf ma

(b) Let's pretend to build mutable lists. Here is a type: `type ListBuilder x a = ([x], a)`

We know, from above, that ListBuilder x is a monad, because [x] is a monoid.

(i) Define

> type ListBuilder x a = ([x], a)

%              v monad

> emit :: x -> ListBuilder x ()
> emit x = ([x], ())

toList :: ListBuilder x ()  -> [x]
toList = fst

reverse :: [a] -> [a]
reverse xs = toList (go xs)
  where
    go :: [a] -> ListBuilder a ()
    go []     = pure ()
    go (x:xs) = do
      go xs
      emit x


2.
(a)(i)

> whenM :: Monad m => m Bool -> m () -> m ()
> whenM mb mx = do
>   b <- mb
>   if b then mx else pure ()

(ii)

> whileM :: Monad m => m Bool -> m ()
> whileM mb = whenM mb (whileM mb)

(iii)

> farewell :: IO ()
> farewell = whileM 
>   (do 
>     putStr "Will you miss me? "
>     answer <- getLine
>     pure (answer /= "yes")
>   )

(b)

> foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
> foldM _ acc []        = pure acc
> foldM mf !acc (x : xs) = do
>   ans <- mf acc x
>   foldM mf ans xs

(c)

> foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
> foldrM _ k [] = pure k
> foldrM mf k (x : xs) = do
>   k' <- foldrM mf k xs
>   mf x k'
