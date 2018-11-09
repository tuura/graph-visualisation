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
drawFlatCircle g = drawFlatCircle' (defaultFlatCircleSettings g) g

-- | Draws the provided graph with each graph vertex at a vertex of a regular polygon with the same number of sides 
drawFlatCircle' :: (Show a, Eq a, Countable a) => Settings a -> Graph a -> Diagram B
drawFlatCircle' s g = connected # frame 0.1
    where connected = foldr (\(a,b) acc -> connectOutside' arrowOpts (name a) (name b) acc) noConnDiag connections
          noConnDiag = atPoints vertices (diag <$> nodes)
          vertices = trailVertices layout
          layout   = layoutPoly $ count g
          (ProcessedGraph nodes connections) = getVertices (s ^. nodeDrawFunction) g
          arrowOpts = with & shaftStyle %~ lw (s ^. dynamicThick) & if (s ^. directed) == Directed then headLength .~ (s ^. dynamicHead) else arrowHead .~ noHead

-- | The default 'Settings' function for this drawing method. By default arrow heads and shafts adapt to the graph size and the graph is 'Directed'.
defaultFlatCircleSettings :: (Countable a, Show a) => Graph a -> Settings a
defaultFlatCircleSettings g = with & dynamicHead .~ (dynamicStyle normal $ count g) 
                                   & dynamicThick .~ (dynamicStyle thin $ count g)