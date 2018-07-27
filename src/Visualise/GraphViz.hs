{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Visualise.GraphViz (
    drawWithGraphViz
) where

import Visualise.Common hiding (Draw,draw)
import Visualise.Tree
import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Data.GraphViz
import Algebra.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

-- | Used to determine how to draw a vertex, different to "Visualise.Common.Draw".
class Draw a where
    -- | Draws a vertex as a Diagram from the vertex contents and if the graph is a graph of graphs, whether the graph contained should be directed.
    draw :: Directed -> a -> Diagram B

-- | Defines how graphs of graphs can be drawn
instance (Show a, Draw a, Eq a, Countable a) => Draw (Graph a) where
    -- | Uses the "Visualise.Tree" module (with the specified settings) to draw each vertex that contains a graph.
    draw dir g = drawTree' (\gr -> Settings (dynamicStyle small $ count gr) 
                                            (dynamicStyle thin $ count gr) 
                                            dir 
                                            (Just 0.2) 
                                            (Just 10) 
                                            (Just 5) 
                                            Nothing 
                                            Nothing
                                            Nothing
                            ) (draw dir) g
-- | Defines how to draw a vertex that has the type of 'String'.
instance Draw String where
    -- | Uses the 'drawGVNode' to draw the vertex, ignoring the 'Directed' parameter.
    draw _ = drawGVNode

-- | Defines how to draw a vertex that has the type of 'Int'.
instance Draw Int where
    -- | Turns the value into a 'String' with 'show' and uses 'drawGVNode' to draw the vertex, ignoring the 'Directed' parameter.
    draw _ = drawGVNode . show

-- | Defines how to compare graphs.
instance (Ord a, Show a) => Ord (Graph a) where
    -- | Simply calls 'show' on the two graphs and compares the resultant 'String's.
    a `compare` b = show a `compare` show b

-- | The default vertex-drawing function for graphs of the type 'String'
drawGVNode :: String -> Diagram B
drawGVNode n = circle 19 <> text n # fontSizeL 20

-- | Uses "Diagrams.TwoD.GraphViz" as an interface to the "Data.GraphViz" library to produce a "Diagrams" representation of the provided graph using the provided "GraphvizCommand".
-- The resultant 'Diagram' is wrapped in an 'IO' from "System.IO" so must be bound to a function such as 'saveSVG' to write it to a file.
-- Uses the arrow paths produced by "GraphViz" but uses the custom functon 'draw' to choose how to draw nodes. To draw graphs of graphs the "Visualise.Tree" module is used for the subgraph layouts.
drawWithGraphViz :: (Ord a, Draw a) => GraphvizCommand  -- ^ The way that "Data.GraphViz" should draw the graph, see "Data.GraphViz.Commands".
                                    -> Directed         -- ^ Whether the graph is 'Directed'.
                                    -> Graph a          -- ^ The graph that should be drawn.
                                    -> IO (Diagram B)   -- ^ The resultant graph drawing in an 'IO' wrapper.
drawWithGraphViz cmd innerDir graph = do
    let g = gen graph
    layedOut <- layoutGraph cmd g
    let drawing = drawGraph (\n p -> place (draw innerDir n) p) (\_ p1 _ p2 _ p -> arrowBetween' (arrowOpts p) p1 p2) layedOut
        arrowOpts p = with & gaps .~ local 24 & headLength .~ local 12 & arrowShaft .~ (unLoc . head . pathTrails $ p)
    return (drawing # frame 20)

-- | Converts a 'Graph' of the type defined by "Algebra.Graph" into a list of vertices and edges to fit "Data.Graph.Inductive.PatriciaTree".
gen :: (Ord a) => Graph a -> Gr a ()
gen g = mkGraph (vertexList g) $ (\(a,b) -> (a,b,())) <$> (edgeList $ g) 

-- testData :: Graph Int
-- testData = (1 * ((2 * ((4 * 7) + (5 * 7))) + (3 * (6 * (5 * 7)))))

testData :: Graph (Graph String)
testData = (Overlay (Connect (Vertex (Vertex "a")) (Vertex (Vertex "b"))) (Overlay (Connect (Vertex (Vertex "b")) (Vertex (Overlay (Vertex "c") (Vertex "d")))) (Connect (Vertex (Overlay (Vertex "c") (Vertex "d"))) (Vertex (Vertex "e")))))

-- testData :: Graph (Graph String)
-- testData = (Overlay (Connect (Vertex (Vertex "a")) (Vertex (Vertex "b"))) (Overlay (Connect (Vertex (Vertex "b")) (Overlay (Vertex (Vertex "c")) (Vertex (Vertex "d")))) (Overlay (Connect (Vertex (Vertex "c")) (Vertex (Vertex "e"))) (Connect (Vertex (Vertex "d")) (Vertex (Vertex "e"))))))

-- testData = (Overlay (Connect (Vertex "a") (Vertex "b")) (Overlay (Connect (Vertex "b") (Overlay (Vertex "c") (Vertex "d"))) (Overlay (Connect (Vertex "c") (Vertex "e")) (Connect (Vertex "d") (Vertex "e")))))