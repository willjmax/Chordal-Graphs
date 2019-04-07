module Lib
    ( graph1
    , empty_graph
    , func
    , grph
    , maxClique
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

cxt  = ([],2,(),[]) :: Context () ()
grph = cxt & empty :: Gr () ()

isChordal :: Gr () () -> Bool
isChordal g = go g 1
    where 
        go g n
            | isEmpty g = True
            | otherwise =
                case match n g of
                    (Just c, g') -> isChordal' r e
                        where r = map fst $ lsuc' c
                              e = edges g
                    (Nothing, g') -> error "Vertex not found"

isChordal' :: [Node] -> [Edge] -> Bool
isChordal' = undefined
        

maxClique :: Gr () () -> Int
maxClique g = go g 1
    where
        go g n
            | isEmpty g = 0
            | otherwise =
                case match n g of
                    (Just c, g') -> max k (go g' (n+1))
                        where k = length $ lsuc' c
                    (Nothing, g') -> error "Vertex not found"

empty_graph :: Gr () () 
empty_graph = empty

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
