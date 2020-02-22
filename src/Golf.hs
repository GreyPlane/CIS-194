{-# OPTIONS_GHC -Wall #-}
module Golf
    (skips)
where

import           Prelude
import Control.Monad

every :: Integer -> [a] -> [a]
every n arr = iter 0 n arr  where
    iter _ _ [] = []
    iter i n' (x : xs) | i == n'   = x : iter 0 n' xs
                       | otherwise = iter (i + 1) n' xs
skips :: [a] -> [[a]]
skips []         = []
skips a@(_ : xs) = everyFirst a : skips xs where everyFirst = every 0

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even

xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x == True then acc + 1 else acc) 0

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]


ex04 = [10,20,30] >>= addOneOrTwo