module JoinList where

import           Prelude
import           Data.Monoid
import           Sized
import           Data.Foldable
data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)
tag :: Monoid m => JoinList m a -> m
tag Empty              = mempty
tag (Single m a      ) = m
tag (Append m lhs rhs) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lhs rhs = Append (tag lhs <> tag rhs) lhs rhs

instance Monoid m => Semigroup (JoinList m a) where
    (<>) = (+++)
instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty
instance Foldable (JoinList m) where
    foldMap f Empty              = mempty
    foldMap f (Single _ a      ) = f a
    foldMap f (Append _ ljl rjl) = foldMap f ljl <> foldMap f rjl

ex01 = do
    ln <- getLine
    print ln