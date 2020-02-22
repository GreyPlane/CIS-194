{-# OPTIONS_GHC -Wall #-}
module Lib
  ( someFunc
  , toDigits
  )
where

import           Log
import           Prelude
import           Control.Monad

someFunc :: IO ()
someFunc = print "./error.log"

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []           = []
doubleEveryOther (x     : []) = [x]
doubleEveryOther (x : y : xs) = x : (y * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits (x : []) = x
sumDigits (x : xs) = x + sumDigits xs

validate :: Integer -> Bool
validate n | ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0 = True
           | otherwise = False

parseMessage :: String -> LogMessage
parseMessage [] = Unknown "null"
parseMessage s@(x : xs)
  | x == 'I' = LogMessage Info
                          (read (unwords (take 1 (words xs))))
                          (unwords (drop 1 (words xs)))
  | x == 'E' = LogMessage (Error (read (unwords (take 1 (words xs)))))
                          (read (unwords (drop 1 (take 2 (words xs)))))
                          (unwords (drop 2 (words xs)))
  | otherwise = Unknown s
parse :: String -> [LogMessage]
parse []  = []
parse str = parseMessage (unwords (take 1 (lines str)))
  : parse (unwords (drop 1 (lines str)))

test :: IO ([LogMessage])
test = testParse parse 10 "./error.log"

insert :: LogMessage -> MessageTree -> MessageTree
insert (     Unknown _       ) tree                       = tree
insert log₁@(LogMessage _ _ _) (Node lht (Unknown _) rht) = Node lht log₁ rht
insert log₂@(LogMessage _ _ _) Leaf                       = Node Leaf log₂ Leaf
insert log@(LogMessage _ t _) (Node lht msg@(LogMessage _ nt _) rht)
  | t > nt    = (Node lht msg (insert log rht))
  | otherwise = (Node (insert log lht) msg rht)

build :: [LogMessage] -> MessageTree
build []       = Leaf
build (l : []) = insert l Leaf
build (l : ls) = insert l (build ls)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf               = []
inOrder (Node lht msg rht) = (inOrder lht) ++ [msg] ++ (inOrder rht)

sort :: [LogMessage] -> [LogMessage]
sort []   = []
sort msgs = inOrder (build msgs)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs []       = xs
diff xs (y : ys) = diff (filter (\x -> not $ x == y) xs) ys

ex01 = unwords . read
ex02 = Just $ 2 + 2
