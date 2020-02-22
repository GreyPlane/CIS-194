{-# LANGUAGE GADTs, DataKinds,
             TypeFamilies, UndecidableInstances #-}
module OddAndEven where

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of even numbers.
data Even (a :: Nat) :: * where
  -- | Zero is even.
  ZeroEven ::Even Z
  -- | If n is even, then n+2 is even.
  NextEven ::Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: * where
  -- | One is odd.
  OneOdd ::Odd (S Z)
  -- | If n is odd, then n+2 is odd.
  NextOdd ::Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven     = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd      = NextEven ZeroEven
oddPlusOne (NextOdd n) = NextEven $ oddPlusOne n

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family   Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Z m = m
type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven     m = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd       om = oddPlusOne om
oddPlusOdd (NextOdd on) om = NextEven (oddPlusOdd on om)

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven      om = om
evenPlusOdd (NextEven en) om = evenPlusOne ((evenPlusOne en) `oddPlusOdd` om)

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
oddPlusEven OneOdd       em = evenPlusOne em
oddPlusEven (NextOdd on) em = NextOdd (oddPlusEven on em)

-- | Multiplies two natural numbers.
type family   Mult (n :: Nat) (m :: Nat) :: Nat
type instance Mult Z m = Z
type instance Mult (S n) m = Add m (Mult n m)

-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven ZeroEven em = ZeroEven
evenTimesEven (NextEven en) em =
  em `evenPlusEven` (em `evenPlusEven` evenTimesEven en em)

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd om = om `oddPlusEven` ZeroEven
oddTimesOdd (NextOdd on) om =
  om `oddPlusEven` (om `oddPlusOdd` oddTimesOdd on om)

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven om = ZeroEven
evenTimesOdd (NextEven en) om =
  om `oddPlusOdd` (om `oddPlusEven` evenTimesOdd en om)

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven OneOdd em = em `evenPlusEven` ZeroEven
oddTimesEven (NextOdd on) em =
  em `evenPlusEven` (em `evenPlusEven` oddTimesEven on em)
