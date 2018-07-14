{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
-- import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as GA

-- connections :: [[Char]]
-- connections = [['b','c','d','e'],['d','e'],['d','e'],['e'],[]]

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

node :: (Show a) => (Int,(a,[a])) -> Diagram B
node (n,(l,_)) = text l # fontSizeL 0.1 <> circle 0.05 # named n

nodeIntToPoint :: [Point v n] -> Int -> Int -> Point v n
nodeIntToPoint (x:xs) a n
    | a == n = x
    | otherwise = nodeIntToPoint xs a (n+1)

arrowPairsToPoints :: (Show a) => [Point v n] -> [(a,a)] -> [(Point v n, Point v n)]
arrowPairsToPoints vertices = foldr (\(x,y) acc -> (nodeIntToPoint vertices (asInt $ show x) 0, nodeIntToPoint vertices (asInt $ show y) 0) : acc) []
    where asInt a = (ord a) - 97

toArrowPairs :: [(a,[a])] -> [(a,a)]
toArrowPairs conns = foldr (\(n,(l,xs)) acc -> (foldr (\y acc2 -> (n,y) : acc2) acc xs)) [] conns

visualiseArrows :: [(Point V2 Double, Point V2 Double)] -> [Diagram B]
visualiseArrows pairs = foldr (\(x,y) acc -> arrowBetween' arrowOpts x y : acc) [] pairs
    where arrowOpts = with & gaps .~ 12

arrows :: ()
arrows = connectOutside 

main = mainWith $ mconcat (atPoints vertices (map node connectionsNum)) : visualiseArrows (arrowPairsToPoints vertices $ toArrowPairs connections)) # frame 1
    where vertices = trailVertices layout
          layout   = layoutPoly $ length connections
          -- numberedV = zip [1..] $ GA.vertexList inputTestData
          connectionsNum = zip [1..] connections 
          connections = GA.adjacencyList inputTestData

getN :: a -> [(Int,(a,[a]))] -> Int
getN a = found
    where Just (found,_) = find (\(x,(y,_) -> y == a)

inputTestData = GA.connect (GA.connect (GA.vertex "a") (GA.overlay (GA.vertex "b") (GA.vertex "c"))) (GA.connect (GA.vertex "d") (GA.vertex "e"))
