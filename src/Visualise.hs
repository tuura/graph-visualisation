module Visualise (
    Method(..),

    drawGraph, drawGraph', saveSVG
) where

import Visualise.Common
import Visualise.Tree
import Visualise.FlatCircle
import Visualise.Hierarchical
import Visualise.FlatAdaptive
import Visualise.GraphViz
import Algebra.Graph
import Diagrams.Prelude
import Diagrams.Backend.SVG

data Method = Tree | TreePartialOrder | Circle | Hierarchical | Adaptive

drawGraph :: (Show a, Eq a, Countable a) => Method -> Graph a -> Diagram B
drawGraph Tree = drawTree
drawGraph TreePartialOrder = drawTreePartialOrder
drawGraph Circle = drawFlatCircle
-- drawGraph Hierarchical = drawHier
drawGraph Adaptive = drawFlatAdaptive

drawGraph' :: (Show a, Eq a, Countable a) => Method -> (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawGraph' Tree = drawTree'
drawGraph' TreePartialOrder = drawTreePartialOrder'
drawGraph' Circle = drawFlatCircle'
-- drawGraph' Hierarchical = drawHier'
drawGraph' Adaptive = drawFlatAdaptive'

saveSVG :: FilePath -> Dimensions -> Diagram B -> IO ()
saveSVG path (w,h) d = renderSVG path (mkSizeSpec2D w h) d