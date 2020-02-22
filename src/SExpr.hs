{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import           AParser
import           Control.Applicative
import           Prelude
import           Data.Char
import           Debug.Trace

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = oneOrMore $ char ' '

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

ex01 = runParser ident "foob123123ar baz nomore"

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

skipSpaces :: Parser ()
skipSpaces = (spaces *> pure ()) <|> pure ()

parseAtom :: Parser Atom
parseAtom = I <$> ident <|> N <$> posInt

parseSExpr :: Parser SExpr
parseSExpr =
  skipSpaces
    *> (   (A <$> parseAtom)
       <|> (Comb <$> (char '(' *> (oneOrMore parseSExpr) <* char ')'))
       )

runPSExpr = runParser parseSExpr
ex02 = runParser
  (char '(' *> (oneOrMore $ skipSpaces *> ident <* skipSpaces) <* char ')')
  "(      ofo123 asd dsds)"
