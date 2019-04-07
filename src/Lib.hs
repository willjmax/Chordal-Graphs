module Lib
    ( graph1
    , empty_graph
    , func
    , grph
    , maxClique
    , isChordal
    , triangle
    , chordal1
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

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

isChordal' :: [Node] -> [Edge] -> Bool
isChordal' [] _  = True
isChordal' (n:ns) es = vChordal n ns && isChordal' ns es
    where
        vChordal n [] = True
        vChordal n (n':ns) = (n, n') `elem` es && vChordal n ns
        
maxClique :: Gr a b -> Int
maxClique g = 1 + go g 1
    where
        go g n
            | isEmpty g = 0
            | otherwise =
                case match n g of
                    (Just c, g') -> max k (go g' (n+1))
                        where k = length $ lsuc' c
                    (Nothing, g') -> error "Vertex not found"

chordal1 :: Gr Char ()
chordal1 = mkGraph [(1, 'a'), (2, 'f'), (3, 'd'), (4, 'h'), (5, 'c'), (6, 'g'), (7, 'b'), (8, 'e')]
                   [(1, 5, ()), (1, 7, ()), (2, 6, ()), (2, 7, ()), (3, 5, ()), (3, 8, ()), (4, 6, ()), (4, 8, ()), (5, 7, ()), (5, 8, ()), (6, 7, ()), (6, 8, ()), (7, 8, ())]

-- a, f, d, h, c, g, b, e

empty_graph :: Gr () () 
empty_graph = empty

triangle :: Gr () ()
triangle = mkGraph [(1, ()), (2, ()), (3, ())]
                   [(1, 2, ()), (1, 3, ()), (2, 3, ())]
                   :: Gr () ()

graph1 = mkGraph [(1, ()), (2, ()), (3, ()), (4, ()), (5, ()), (6, ())] 
                 [(1, 2, ()), (1, 4, ()), (2, 3, ()), (2, 4, ()), (2, 5, ()), (2, 6, ())]
                 :: Gr () ()

func :: Gr () () -> Int
func g
    | isEmpty g = 0
    | otherwise = 1 + func g'
    where
        n  = noNodes g    
        v  = context g n
        g' = delNode n g
