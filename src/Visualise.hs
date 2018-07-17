module Visualise (
    ProcessedGraph(..),

    Dimensions, ConnectList,

    saveSVG, countVertices, getVertices, connectedFrom, connectedTo, node, dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG
import Data.List

data ProcessedGraph a = ProcessedGraph [a] [(a, a)] deriving (Show)

type Dimensions = (Maybe Double, Maybe Double)

type ConnectList a = [(a,[a])]

saveSVG :: FilePath -> Dimensions -> Diagram B -> IO ()
saveSVG path (w,h) d = renderSVG path (mkSizeSpec2D w h) d

countVertices :: Graph a -> Measure Double
countVertices Empty = 1
countVertices (Vertex a) = 1
countVertices (Overlay a b) = countVertices a + countVertices b
countVertices (Connect a b) = countVertices a + countVertices b

getVertices :: Graph String -> ProcessedGraph String
getVertices g = namesAndConnections g ""

namesAndConnections :: Graph String -> String -> ProcessedGraph String
namesAndConnections Empty c = ProcessedGraph ["_empty_node_" ++ c] []
namesAndConnections (Vertex a) c = ProcessedGraph [a] []
namesAndConnections (Overlay a b) c = ProcessedGraph (nA ++ nB) (cA ++ cB)
    where (ProcessedGraph nA cA) = namesAndConnections a (c ++ "_l")
          (ProcessedGraph nB cB) = namesAndConnections b (c ++ "_r")
namesAndConnections (Connect a b) c = ProcessedGraph (nA ++ nB) ([(aA, bB) | aA <- nA, bB <- nB] ++ cA ++ cB)
    where (ProcessedGraph nA cA) = namesAndConnections a (c ++ "_l")
          (ProcessedGraph nB cB) = namesAndConnections b (c ++ "_r")

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

node :: Double -> Double -> String -> Diagram B
node fS cS n = (text nodeText # fontSizeL fS # href ("javascript:alert(\"Node " ++ n ++ "\")") <> circle cS) # named n
    where nodeText = if "_empty_node_" `isPrefixOf` n then "" else n

dynamicStyle :: Measure Double -> Measure Double -> Measure Double
dynamicStyle def graphSize = def * 10/graphSize

