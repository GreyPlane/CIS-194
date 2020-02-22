{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Proof where

data Z
data S n

data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)


-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive ( NumS x ) = EqlS $ reflexive x 

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric ( EqlS n ) = EqlS $ symmetric n 

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS x) (EqlS y) = EqlS $ transitive x y

plusIdentityR :: Equal a a -> Equal (a :+: Z) a
plusIdentityR EqlZ  = EqlZ
plusIdentityR (EqlS n) = EqlS $ plusIdentityR n 

plusSuc :: Natural a -> Natural b -> Equal (a :+: S b) (S (a :+: b))
plusSuc NumZ n = EqlS $ reflexive n
plusSuc (NumS m) n = EqlS $ plusSuc m n

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes x NumZ = plusIdentityR $ reflexive x
plusCommutes x (NumS y) = plusSuc x y `transitive` (symmetric ( EqlS $ plusCommutes y x ))