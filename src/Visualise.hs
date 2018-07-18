module Visualise (
    ProcessedGraph(..),

    Dimensions, ConnectList,

    saveSVG, countVertices, getVerticesString, getVerticesChar, getVerticesInt, getVerticesGraph, connectedFrom, connectedTo, node, dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude hiding (Empty, union)
import Diagrams.Backend.SVG
import Data.List

data ProcessedGraph a = ProcessedGraph [a] [(a, a)] deriving (Show)

type Dimensions = (Maybe Double, Maybe Double)

type ConnectList a = [(a,[a])]

saveSVG :: FilePath -> Dimensions -> Diagram B -> IO ()
saveSVG path (w,h) d = renderSVG path (mkSizeSpec2D w h) d

countVertices :: Graph a -> Int
countVertices Empty = 1
countVertices (Vertex a) = 1
countVertices (Overlay a b) = countVertices a + countVertices b
countVertices (Connect a b) = countVertices a + countVertices b

getVerticesString :: Graph String -> ProcessedGraph String
getVerticesString g = namesAndConnections id g ""

getVerticesChar :: Graph Char -> ProcessedGraph String
getVerticesChar g = namesAndConnections (id . flip (:) []) g ""

getVerticesInt :: Graph Int -> ProcessedGraph String
getVerticesInt g = namesAndConnections show g ""

getVerticesGraph :: Graph (Graph Int) -> ProcessedGraph String -- TODO: Not just Int
getVerticesGraph g = namesAndConnections (\a -> let ProcessedGraph names connections = getVerticesInt a in mconcat names) g ""

namesAndConnections :: (a -> String) -> Graph a -> String -> ProcessedGraph String
namesAndConnections displayF Empty c = ProcessedGraph ["_empty_node_" ++ c] []
namesAndConnections displayF (Vertex a) c = ProcessedGraph [displayF a] []
    -- | isGraph a = namesAndConnections a c
    -- | otherwise = ProcessedGraph [show a] []
-- namesAndConnections (Vertex a) c = ProcessedGraph [a] []
namesAndConnections displayF (Overlay a b) c = ProcessedGraph (nA `union` nB) (cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections displayF a (c ++ "_l")
          (ProcessedGraph nB cB) = namesAndConnections displayF b (c ++ "_r")
namesAndConnections displayF (Connect a b) c = ProcessedGraph (nA `union` nB) ([(aA, bB) | aA <- nA, bB <- nB] `union` cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections displayF a (c ++ "_l")
          (ProcessedGraph nB cB) = namesAndConnections displayF b (c ++ "_r")

-- isGraph :: a -> Bool
-- isGraph Empty         = True
-- isGraph (Vertex _)    = True
-- isGraph (Overlay _ _) = True
-- isGraph (Connect _ _) = True
-- isGraph _             = False

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
node fS cS n = (text nodeText # fontSizeL fS <> circle cS) # named n # href ("javascript:alert(\"Node " ++ n ++ "\")")
    where nodeText = if "_empty_node_" `isPrefixOf` n then "" else n

dynamicStyle :: Measure Double -> Int -> Measure Double
dynamicStyle def graphSize = def * 10/fromIntegral graphSize

