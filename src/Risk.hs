{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
 deriving (Show)


countAtk :: Army -> (Army, Int)
countAtk atkers = _count atkers 0 where
  _count 1 times = (1, times)
  _count n 3     = (n, 3)
  _count n times = _count (n - 1) (times + 1)

countDef :: Army -> (Army, Int)
countDef defers = _count defers 0 where
  _count 0 times = (0, times)
  _count n 2     = (n, 2)
  _count n times = _count (n - 1) (times + 1)

maxDV x y = DV $ max (unDV x) (unDV y)

zipDie :: [DieValue] -> [DieValue] -> [(DieValue, DieValue)]
zipDie atks defs
  | lenA == lenD = zip atks defs
  | lenA > lenD  = let diff = lenA - lenD in zip (prep atks diff) defs
  | lenA < lenD  = let diff = lenD - lenA in zip atks (prep defs diff) where
  lenA = length atks
  lenD = length defs
  prep vs n = (fst v) ++ [(foldl maxDV 0 (snd v))] where v = splitAt n vs

applyResult :: [(DieValue, DieValue)] -> Battlefield -> Battlefield
applyResult vs bf = _build vs (0, 0) where
  _build [] (a, b) = Battlefield (attackers bf - a) (defenders bf - b)
  _build ((atk, def) : ys) (a, b) | atk > def = _build ys (a, b + 1)
                                  | otherwise = _build ys (a + 1, b)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  atkV   <- forM [1 .. (attackers b)] (const die)
  defV   <- forM [1 .. (defenders b)] (const die)
  result <- return $ zipDie atkV defV
  return $ applyResult result b

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  (restA, atkers) <- return $ countAtk (attackers b)
  (restD, defers) <- return $ countDef (defenders b)
  if (atkers + restA > 1 && defers + restD > 0)
    then do
      result <- battle (Battlefield atkers defers)
      invade $ Battlefield (attackers result + restA) (defenders result + restD)
    else do
      return b

successProb :: Battlefield -> Rand StdGen Double
successProb b = _count b 0 (0, 0) where
  _count _  1000 (a, _) = return $ fromIntegral a / 1000
  _count bf n    (a, b) = do
    result <- battle bf
    if (attackers result > defenders result)
      then do
        _count bf (n + 1) (a + 1, b)
      else do
        _count bf (n + 1) (a, b + 1)

ex = evalRandIO $ successProb (Battlefield 10 10)