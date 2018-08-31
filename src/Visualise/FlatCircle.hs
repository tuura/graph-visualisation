{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module: Visualise.FlatCircle
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Draws a graph with each vertex at a vertex of a regular polygon with the
-- same number of sides as the graph does vertices.
--
-- Provides a function which draws the graph with default 'Settings', 
-- 'drawFlatCircle', and a second function which draws the graph with custom
-- 'Settings'.
--
-----------------------------------------------------------------------------
module Visualise.FlatCircle (
    -- * Draws a graph with default 'Settings'.
    drawFlatCircle, 

    -- * Draws a graph with customised 'Settings'.
    drawFlatCircle'
) where

import Algebra.Graph
import Visualise.Common
import Diagrams.Prelude     hiding (Empty)
import Diagrams.Backend.SVG
import Diagrams.Path
import Data.Char
import Data.List

-- | Produces a polygon trail with a given number of sides.
layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

-- | Draws the provided graph as a circle with default settings.
drawFlatCircle :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawFlatCircle g = drawFlatCircle' (defaultFlatCircleSettings g) drawDefaultNode g

-- | Draws the provided graph with each graph vertex at a vertex of a regular polygon with the same number of sides 
drawFlatCircle' :: (Show a, Eq a, Countable a) => Settings -> (a -> Diagram B) -> Graph a -> Diagram B
drawFlatCircle' s drawF g = connected # frame 0.1
    where connected = foldr (\(a,b) acc -> connectOutside' arrowOpts (name a) (name b) acc) noConnDiag connections
          noConnDiag = atPoints vertices (diag <$> nodes)
          vertices = trailVertices layout
          layout   = layoutPoly $ count g
          (ProcessedGraph nodes connections) = getVertices drawF g
          arrowOpts = with & shaftStyle %~ lw (dynamicThick s) & if directed s == Directed then headLength .~ dynamicHead s else arrowHead .~ noHead

-- | The default 'Settings' function for this drawing method. By default arrow heads and shafts adapt to the graph size and the graph is 'Directed'.
defaultFlatCircleSettings :: (Countable a) => Graph a -> Settings
defaultFlatCircleSettings g = Settings (dynamicStyle normal $ count g)
                             (dynamicStyle thin $ count g)
                             Directed
                             Nothing
                             Nothing
                             Nothing
                             Nothing
                             Nothing
                             Nothing
                             Nothing

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 7) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
inputTestData = (Connect (Vertex 2) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))

