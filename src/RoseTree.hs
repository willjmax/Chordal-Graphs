module RoseTree where


data RTree a = Empty | Node a [RTree a]
               deriving Show


foldR :: (a -> [b] -> b) -> RTree a -> b
foldR f (Node x ts) = f x (map (foldR f) ts)

mapR :: (a -> b) -> RTree a -> RTree b
mapR f = foldR (Node . f)

dfs :: RTree a -> [a]
dfs (Node x ts) = x:concatMap dfs ts
-- dfs = foldR (\x xs->x:concat xs)
-- dfs = foldR (\x->(x:) . concat)


lf x = Node x []

t = Node 3 [lf 1, lf 5, Node 11 [lf 7, lf 8], lf 14]
