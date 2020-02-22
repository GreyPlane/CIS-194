module Stream where

import           Control.Arrow
import           Control.Applicative

data Stream a = a :> Stream a

headS :: Stream a -> a
headS (x :> _) = x

tailS :: Stream a -> Stream a
tailS (_ :> xs) = xs

-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)
ex = takeS 10 $ iterateS (1 +) 0
-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS [] = error "no"
cycleS xs = (\x -> xs !! (x `rem` (length xs))) <$> (fromS 0)

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS = iterateS (1 +)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = iterateS (s +) x

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) | p x == False = filterS p xs
                    | otherwise    = x :> filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i _ | i <= 0 = []
takeS 1 s          = headS s : []
takeS i s          = headS s : takeS (i - 1) (tailS s)

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s | i <= 0 = s
dropS 1 s = tailS s
dropS i s = dropS (i - 1) (tailS s)
ex03 = headS $ dropS 10 $ fromS 0

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (takeS i s, dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f xs ys = f (headS xs) (headS ys) :> zipWithS f (tailS xs) (tailS ys)

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure a = a :> pure a

-- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) fs xs = ((headS fs) (headS xs)) :> ((tailS fs) <*> (tailS xs))

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> (1 :> (liftA2 (+) fibS (tailS fibS)))

minusS :: Ord a => Stream a -> Stream a -> Stream a
minusS (x :> xs) (y :> ys) = case (compare x y) of
    LT -> x :> minusS xs (y :> ys)
    EQ -> minusS xs ys
    GT -> minusS (x :> xs) ys
-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = sieve (fromS 2)
    where sieve (p :> xs) = p :> sieve (filterS (\x -> x `mod` p > 0) xs)
ex01 = takeS 2 primeS
