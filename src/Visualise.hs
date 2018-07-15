module Visualise (
    ProcessedGraph(..),

    Dimensions, ConnectList,

    draw, countVertices, getVertices, connectedFrom, connectedTo, dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.List

data ProcessedGraph a = ProcessedGraph [a] [(a, a)] deriving (Show)

type Dimensions = (Maybe Double, Maybe Double)

type ConnectList a = [(a,[a])]

draw :: FilePath -> Dimensions -> Diagram SVG -> IO ()
draw path (w,h) = renderSVG path (mkSizeSpec2D w h)

countVertices :: Graph a -> Measure Double
countVertices (Vertex a) = 1
countVertices (Overlay a b) = countVertices a + countVertices b
countVertices (Connect a b) = countVertices a + countVertices b

getVertices :: (Show a, Eq a) => Graph a -> ProcessedGraph a
getVertices (Vertex a) = ProcessedGraph [a] []
getVertices (Overlay a b) = ProcessedGraph (nA ++ nB) (cA ++ cB)
    where (ProcessedGraph nA cA) = getVertices a
          (ProcessedGraph nB cB) = getVertices b
getVertices (Connect a b) = ProcessedGraph (nA ++ nB) ([(aA, bB) | aA <- nA, bB <- nB] ++ cA ++ cB)
    where (ProcessedGraph nA cA) = getVertices a
          (ProcessedGraph nB cB) = getVertices b

connectedFrom :: (Show a, Eq a) => [(a,a)] -> ConnectList a
connectedFrom [(a,b),(x,y)]
  | b == y = [(b,[a,x])]
  | otherwise = (b,[a]) : [(y,[x])]
connectedFrom l@((x,y):zs) = (y,incoming) : if not (null remaining) then connectedFrom remaining else []
    where remaining = zs \\ filtered
          incoming = foldr (\(a,b) acc -> a : acc) [] filtered
          filtered = filter (\(a,b) -> b == y) l

connectedTo :: (Show a, Eq a) => [(a,a)] -> ConnectList a
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if not (null remaining) then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l


dynamicStyle :: Measure Double -> Measure Double -> Measure Double
dynamicStyle def graphSize = def * 10/graphSize

