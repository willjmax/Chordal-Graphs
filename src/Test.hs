module Test where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

chordal1 :: Gr Char ()
chordal1 = mkGraph [(1, 'a'), (2, 'f'), (3, 'd'), (4, 'h'), (5, 'c'), (6, 'g'), (7, 'b'), (8, 'e')]
                   [(1, 5, ()), (1, 7, ()), (2, 6, ()), (2, 7, ()), (3, 5, ()), (3, 8, ()), (4, 6, ()), (4, 8, ()), (5, 7, ()), (5, 8, ()), (6, 7, ()), (6, 8, ()), (7, 8, ())]

triangle :: Gr () ()
triangle = mkGraph [(1, ()), (2, ()), (3, ())]
                   [(1, 2, ()), (1, 3, ()), (2, 3, ())]

graph1 = mkGraph [(1, ()), (2, ()), (3, ()), (4, ()), (5, ()), (6, ())] 
                 [(1, 2, ()), (1, 4, ()), (2, 3, ()), (2, 4, ()), (2, 5, ())]
                 :: Gr () ()
