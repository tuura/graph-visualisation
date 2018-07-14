module Visualise (
    ProcessedGraph(..),

    countVertices, getVertices, dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude
import Diagrams.Backend.SVG

data ProcessedGraph a = ProcessedGraph [a] [(a, a)] deriving (Show)

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


dynamicStyle :: Measure Double -> Measure Double -> Measure Double
dynamicStyle def graphSize = def * 10/graphSize