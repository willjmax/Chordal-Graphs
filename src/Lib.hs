{-# LANGUAGE RecordWildCards #-}

module Lib
    ( maxClique
    , maxCliqueFold
    , maxCliqueSize
    , maxCliqueSizeFold
    , isChordal
    , chordalCompletion
    , sucNeighbors
    , testTD
    , addToBag
    , addSucs
    , getSucSet
    , buildTreeDecomp
    , treeDecomp
    , getTreeDecomp
    , TDState
    , roseTreeDecomp
    ) where

import Control.Monad.State
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List
import Data.Set (Set)
import RoseTree
import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as Set

type Bag        = Set Node
type TreeDecomp = Gr Bag ()
type SucSet     = Node -> Set Node

-- TDState contains: 
--      the mapping of nodes to their bags
--      the set of successors for each node in g
--      the partial tree decomposition
--      the current bag being processed

--type TDState    = (IM.IntMap Node, SucSet, TreeDecomp, Node)
data TDState = TDState {
    bagMap    :: IM.IntMap Node,
    sucMap    :: SucSet,
    partialTD :: TreeDecomp,
    currBag   :: Node
}

type RTreeDecomp = RTree Bag

-- the node parameter is the root of the tree
-- assumes an undirected graph, hence the children are taken from both predecessors and successors
-- the map logic only works under the assumption that the input is a tree
roseTreeDecomp :: TreeDecomp -> Node -> RTreeDecomp
roseTreeDecomp td n
    | isEmpty td = Empty
    | otherwise  = case match n td of
                    (Just c, td') -> Node (lab' c) (map (roseTreeDecomp td') children)
                        where children = pre' c ++ suc' c
                    (Nothing, _)  -> error "node not found"

getTreeDecomp :: TDState -> TreeDecomp
getTreeDecomp = partialTD

testTD = mkGraph [(1, Set.fromList [1, 2, 3, 4]), (2, Set.fromList [2, 3]), (3, Set.fromList [3, 4]), (4, Set.fromList [1, 4])]
                 [(1, 2, ()), (2, 3, ()), (3, 4, ())]
                 :: TreeDecomp

--  i: index of the current vertex being processed
--  k: index of current bag being populated
-- td: the tree decomposition
--  b: current bag being populated
--  t: a function Node -> Node that maps a node of g to it's corresponding bag in td

buildTreeDecomp :: Gr a b -> State TDState ()
buildTreeDecomp g
    | isEmpty g = return ()
    | otherwise = do
                    TDState{..} <- get
                    let (c,g') = matchLargest g -- process elimination ordering backwards
                        i      = node' c        -- current node of g being processed
                        --bi     = ss i           -- the new bag
                        bi     = sucMap i
                        m      = minimum bi      -- minimum successor of i
                        tm     = case IM.lookup m bagMap of
                                    Just l   -> l
                                    Nothing  -> error "tm: Bag not found"
                        bm     = case match tm partialTD of
                                    (Just c,_)   -> lab' c
                                    (Nothing,_)  -> error "bm: Bag not found" 
                    if bi == bm
                    then
                        let bagMap' = IM.insert i tm bagMap
                            partialTD' = addToBag partialTD tm i
                        in put TDState{bagMap=bagMap',sucMap=sucMap,partialTD=partialTD',currBag=currBag}
                    else
                        let currBag'  = currBag + 1
                            bk' = Set.insert i bi
                            bagMap' = IM.insert i currBag' bagMap
                            partialTD' = ([], currBag', bk', [((), tm)]) & partialTD
                        in put TDState{bagMap=bagMap',sucMap=sucMap,partialTD=partialTD',currBag=currBag'}
                    buildTreeDecomp g'

treeDecomp :: Gr a b -> ((), TDState)
treeDecomp g = runState (buildTreeDecomp g') TDState{ bagMap=IM.singleton n 1
                                                    , sucMap=getSucSet g
                                                    , partialTD =mkGraph [(1, Set.singleton n)] []
                                                    , currBag=1
                                             }
    where n  = noNodes g
          g' = delNode n g

-- get all neighbors of a vertex v that are also successors of v in the elimination ordering
sucNeighbors :: Gr a b -> Node -> [Node]
sucNeighbors g n = filter (\v -> v `elem` suc g n) (neighbors g n)

-- add vertex to bag      Bag Index Vertex
addToBag :: TreeDecomp -> Node -> Node -> TreeDecomp
addToBag td b v = insNode (b, lb') (delNode b td)
    where lb  = case lab td b of (Just l) -> l -- TreeDecomp type should make this safe
          lb' = Set.insert v lb

isChordal :: Gr a b -> Bool
isChordal g = go g 1
    where 
        go g n
            | isEmpty g = True
            | otherwise =
                case match n g of
                    (Just c, g')  -> isChordal' r e && go g' (n+1)
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

--isChordalFold :: Gr a b -> Bool
--isChordalFold = ufold (\c -> True) True

-- helper function for chordalCompletion
addClique :: Gr a () -> [Node] -> Gr a ()
addClique g []     = g
addClique g (n:ns) = addClique gc ns
    where gc = insEdges fs g
          es = labEdges g
          fs = [x | x <- zip3 (repeat n) ns (repeat ()), not (x `elem` es)]

chordalCompletion :: Gr a () -> Gr a ()
chordalCompletion g = ufold chordalCompletion' g g

chordalCompletion' :: Context a () -> Gr a () -> Gr a ()
chordalCompletion' c g = addClique g (map fst $ lsuc' c)

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

ofold :: (Graph gr) => (Context a b -> c -> c) -> c -> gr a b -> c
ofold f u g = ofold' f u g (nodes g)

ofold' :: (Graph gr) => (Context a b -> c -> c) -> c -> gr a b -> [Node] -> c
ofold' _ u _ []     = u
ofold' f u g (n:ns) = f c (ofold' f u g ns)
    where c = context g n

type NodeMap = Node -> Int

inc :: NodeMap -> Node -> NodeMap
inc f n = \x -> if x == n then (f n) + 1 else f n

incr :: NodeMap -> [Node] -> NodeMap
incr f ns = \n -> if n `elem` ns then (f n) + 1 else f n

incrBy :: NodeMap -> Node -> Int -> NodeMap
incrBy f v x = \n -> if v == n then (f n) + x else f n

degrees :: Gr a b -> NodeMap
degrees = ufold (\c f -> incrBy (incr f (suc' c ++ pre' c)) (node' c) (deg' c)) (const 0)

maxDegree :: Gr a b -> Int
maxDegree g = maximum (map f (nodes g))
    where f = degrees g

ufold' :: (Graph gr) => (gr a b -> GDecomp gr a b) -> (Context a b -> c -> c) -> c -> gr a b -> c
ufold' m f u g
    | isEmpty g = u
    | otherwise = f c (ufold' m f u g')
    where
        (c,g') = m g

matchLargest :: (Graph gr) => gr a b -> GDecomp gr a b
matchLargest g = case (reverse $ nodes g) of
                   []    -> error "Match Exception, Empty Graph"
                   (v:_) -> (c,g')
                     where
                       (Just c,g') = match v g

rfold :: (Graph gr) => (Context a b -> c -> c) -> c -> gr a b -> c
rfold f u g
    | isEmpty g = u
    | otherwise = f c (rfold f u g')
    where
        (c,g') = matchLargest g

addSucs :: SucSet -> Node -> [Node] -> SucSet
addSucs f n ns = \x -> if x `elem` ns then Set.insert n (f x) else f x

getSucSet :: Gr a b -> SucSet
getSucSet = rfold (\c f -> addSucs f (node' c) (pre' c)) (const Set.empty)
