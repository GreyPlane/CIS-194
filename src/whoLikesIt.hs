module WhoLikesIt where
import           Prelude
import           Data.Char

likes :: [String] -> String
likes []              = "no one likes this"
likes (   x     : []) = x ++ " likes this"
likes (   x : y : []) = unwords $ x : "and" : y : "like this" : []
likes xs@(x : y : zs) = x ++ ", " ++ likes core  where
    core = case length zs of
        1 -> tail xs
        _ -> y : ( len ++ " others" ) : [] where len = show (length zs)


