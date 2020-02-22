{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import           ExprT
import           Parser
import           StackVM
import           Control.Monad

eval :: ExprT -> Integer
eval (Lit num            ) = num
eval (ExprT.Mul exp1 exp2) = eval exp1 * eval exp2
eval (ExprT.Add exp1 exp2) = eval exp1 + eval exp2

evalStr :: String -> Maybe Integer
evalStr str = evalM (parseExp Lit ExprT.Add ExprT.Mul str)
  where
    evalM (Just exps) = Just $ eval exps
    evalM Nothing     = Nothing
evalStr' :: String -> Maybe Integer
evalStr' str = ap (Just eval) (parseExp Lit ExprT.Add ExprT.Mul str)
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = ExprT.Add
    mul = ExprT.Mul
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)
instance Expr Bool where
    lit x | x <= 0    = False
          | otherwise = True
    add = (||)
    mul = (&&)
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min
newtype Mod7 = Mod7 Integer deriving (Num)
instance Expr Mod7 where
    lit = Mod7 . mod 7
    add = (+)
    mul = (*)
instance Expr Program where
    lit x = [PushI x]

    add lhs rhs = lhs ++ rhs ++ [StackVM.Add]

    mul lhs rhs = lhs ++ rhs ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
