{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Visualise.GraphViz () where

import Visualise.Common hiding (Draw,draw)
import Visualise.Tree
import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Data.GraphViz
import Algebra.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)

-- main = mainWith $ theGraph >>= drawGraph
  -- where --theGraph :: IO (Diagram B)

-- main = mainWith ex1

class Draw a where
    draw :: Directed -> a -> Diagram B

instance (Show a, Draw a) => Draw (Graph a) where
    draw dir g = drawTree' (\gr -> Settings (dynamicStyle small $ count gr) (dynamicStyle thin $ count gr) dir (Just 0.2) (Just 10) (Just 5) Nothing Nothing Nothing) drawGVNode $ show <$> g

instance Draw String where
    draw _ = drawGVNode

instance Draw Int where
    draw _ = drawGVNode . show

instance (Ord a, Show a) => Ord (Graph a) where
    a `compare` b = show a `compare` show b

drawGVNode :: String -> Diagram B
drawGVNode n = circle 19 <> text n # fontSizeL 20

drawWithGraphViz :: (Ord a, Draw a) => GraphvizCommand -> Directed -> Graph a -> IO (Diagram B)
drawWithGraphViz cmd innerDir graph = do
    let g = gen graph
    layedOut <- layoutGraph cmd g
    let drawing = drawGraph (\n p -> place (draw innerDir n) p) (\_ p1 _ p2 _ p -> arrowBetween' (arrowOpts p) p1 p2) layedOut
        arrowOpts p = with & gaps .~ local 24 & headLength .~ local 12 & arrowShaft .~ (unLoc . head . pathTrails $ p)
    return (drawing # frame 20)

gen :: (Ord a) => Graph a -> Gr a ()
gen g = mkGraph (vertexList g) $ (\(a,b) -> (a,b,())) <$> (edgeList $ g) 

-- testData :: Graph Int
-- testData = (1 * ((2 * ((4 * 7) + (5 * 7))) + (3 * (6 * (5 * 7)))))

testData :: Graph (Graph String)
testData = (Overlay (Connect (Vertex (Vertex "a")) (Vertex (Vertex "b"))) (Overlay (Connect (Vertex (Vertex "b")) (Vertex (Overlay (Vertex "c") (Vertex "d")))) (Connect (Vertex (Overlay (Vertex "c") (Vertex "d"))) (Vertex (Vertex "e")))))

-- testData :: Graph (Graph String)
-- testData = (Overlay (Connect (Vertex (Vertex "a")) (Vertex (Vertex "b"))) (Overlay (Connect (Vertex (Vertex "b")) (Overlay (Vertex (Vertex "c")) (Vertex (Vertex "d")))) (Overlay (Connect (Vertex (Vertex "c")) (Vertex (Vertex "e"))) (Connect (Vertex (Vertex "d")) (Vertex (Vertex "e"))))))

-- testData = (Overlay (Connect (Vertex "a") (Vertex "b")) (Overlay (Connect (Vertex "b") (Overlay (Vertex "c") (Vertex "d"))) (Overlay (Connect (Vertex "c") (Vertex "e")) (Connect (Vertex "d") (Vertex "e")))))