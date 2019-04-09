# Chordal.hs
A library for Chordal graph algorithms in Haskell using the FGL library.
All graphs are assumed to be given as an elimination ordering
We provide implementations of algorithms on Chordal graphs for problems that are generally NP-Complete on general graphs

# Current implemented functions:
`isChordal :: Gr a b -> Bool`
Checks if an elimination ordering is perfect

`chordalCompletion :: Gr a () -> Gr a ()`
Takes an elimination ordering of a graph and returns its chordal completion

`maxClique :: Gr a b -> Int`
Returns the size of a maximum clique in the graph
