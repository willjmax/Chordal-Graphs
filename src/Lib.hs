module Lib
    ( graph1
    , grph
    , maxClique
    , maxCliqueFold
    , maxCliqueSize
    , maxCliqueSizeFold
    , isChordal
    , triangle
    , chordal1
    , chordalCompletion
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type TreeDecomp = Gr [Node] ()

cxt  = ([],2,(),[]) :: Context () ()
grph = cxt & empty :: Gr () ()

isChordal :: Gr a b -> Bool
isChordal g = go g 1
    where 
        go g n
            | isEmpty g = True
            | otherwise =
                case match n g of
                    (Just c, g') -> isChordal' r e && go g' (n+1)
                        where r = map fst $ lsuc' c
                              e = edges g
                    (Nothing, g') -> error "Vertex not found"

-- helper function for isChordal
isChordal' :: [Node] -> [Edge] -> Bool
isChordal' [] _  = True
isChordal' (n:ns) es = vChordal n ns && isChordal' ns es
    where
        vChordal n [] = True
        vChordal n (n':ns) = (n, n') `elem` es && vChordal n ns
        
chordalCompletion :: Gr a () -> Gr a ()
chordalCompletion g = go g 1
    where
        go g n
            | isEmpty g = g
            | otherwise =
                case match n g of
                    (Just c, _) -> go gc (n+1)
                        where ns = map fst $ lsuc' c
                              gc = chordalCompletion' g ns
                    (Nothing, _) -> g

-- helper function for chordalCompletion
chordalCompletion' :: Gr a () -> [Node] -> Gr a ()
chordalCompletion' g []     = g
chordalCompletion' g (n:ns) = chordalCompletion' gc ns
    where gc = insEdges fs g
          es = labEdges g
          fs = [x | x <- zip3 (repeat n) ns (repeat ()), not (x `elem` es)]

--chordalCompletionFold :: Gr a () -> Gr a ()
--chordalCompletionFold = ufold 

maxCliqueSize :: Gr a b -> Int
maxCliqueSize g = 1 + go g 1
    where
        go g n
            | isEmpty g = 0
            | otherwise =
                case match n g of
                    (Just c, g') -> max k (go g' (n+1))
                        where k = length $ lsuc' c
                    (Nothing, _) -> error "Vertex not found"

maxCliqueSizeFold :: Gr a b -> Int
maxCliqueSizeFold g = 1 + ufold (\c k -> max (length $ lsuc' c) k) 0 g

maxClique :: Gr a b -> [Node]
maxClique g = go g 1 []
    where
        go g n ns
            | isEmpty g = ns
            | otherwise = 
                case match n g of
                    (Just c, g') -> if k' > k then go g (n+1) ns' else go g (n+1) ns
                        where ns' = n : suc' c
                              k   = length ns
                              k'  = length ns'
                    (Nothing, _) -> ns

maxCliqueFold :: Gr a b -> [Node]
maxCliqueFold = ufold maxCliqueFold' []

maxCliqueFold' :: Context a b -> [Node] -> [Node]
maxCliqueFold' c ns = if k' > k then ns' else ns
    where ns' = n : suc' c
          k   = length ns
          k'  = length ns'
          n   = node' c

chordal1 :: Gr Char ()
chordal1 = mkGraph [(1, 'a'), (2, 'f'), (3, 'd'), (4, 'h'), (5, 'c'), (6, 'g'), (7, 'b'), (8, 'e')]
                   [(1, 5, ()), (1, 7, ()), (2, 6, ()), (2, 7, ()), (3, 5, ()), (3, 8, ()), (4, 6, ()), (4, 8, ()), (5, 7, ()), (5, 8, ()), (6, 7, ()), (6, 8, ()), (7, 8, ())]

triangle :: Gr () ()
triangle = mkGraph [(1, ()), (2, ()), (3, ())]
                   [(1, 2, ()), (1, 3, ()), (2, 3, ())]

graph1 = mkGraph [(1, ()), (2, ()), (3, ()), (4, ()), (5, ()), (6, ())] 
                 [(1, 2, ()), (1, 4, ()), (2, 3, ()), (2, 4, ()), (2, 5, ()), (2, 6, ())]
                 :: Gr () ()
