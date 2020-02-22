module Party where
import           Employee
import           Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps totalFun) = GL (emp : emps) (totalFun + empFun emp)

instance Semigroup GuestList where
    (<>) (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)
instance Monoid GuestList where
    mempty  = GL [] 0
    mappend = (<>)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l r | l > r     = l
            | otherwise = r

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node v nodes) | length nodes == 0 = e
                            | otherwise         = f v $ map (treeFold e f) nodes
ex = treeFold 0 (\x xs -> 1 + empFun x + sum xs) testCompany

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss lists =  ((foldr moreFun mempty $ fst $ unzip lists),  (foldr moreFun mempty $ snd $ unzip lists))
