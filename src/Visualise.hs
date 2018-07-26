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
import Data.GraphViz.Commands

-- | The graph drawing method
data Method = Tree  -- ^ The 'drawTree' function from "Visualise.Tree" module will be used to draw the graph
            | TreePartialOrder  -- ^ The 'drawTreePartialOrder' function "Visualise.Tree" module will be used to draw the graph, with all indirect dependencies removed by using the Coffman-Graham algorithm
            | Circle  -- ^ The "Visualise.FlatCircle" module will be used to draw the graph, with the verticies being placed on the vertecies of a regular polygon of n sides (where n is the number of graph vertices)
            | Hierarchical  -- ^ The "Visualise.Hierarchical" module will be used to draw the graph as a hierarchical graph by grouping together vertices and groups of vertices with common connections
            | Adaptive  -- ^ The "Visualise.FlatAdaptive" module will be used to draw the graph, with the layout of vertices being dynamic depending on the connections between them.

-- | The 'Dimensions' data type is used to store the dimensions for writing a 'Diagram' to a file. Either the width or height (or both) can be provided in a maybe tuple in the order (Width, Height).
type Dimensions = (Maybe Double, Maybe Double)

-- | Draw a graph using the specified method, uses default settings
drawGraph :: (Show a, Eq a, Countable a) => Method -> Graph a -> Diagram B
drawGraph Tree = drawTree
drawGraph TreePartialOrder = drawTreePartialOrder
drawGraph Circle = drawFlatCircle
drawGraph Hierarchical = drawHier
drawGraph Adaptive = drawFlatAdaptive

-- | Draw a graph using the specified method, but also using the specified settings (provided as a function that will take a graph and return a 'Settings' instance, see "Visualise")
drawGraph' :: (Show a, Eq a, Countable a) => Method -> (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawGraph' Tree = drawTree'
drawGraph' TreePartialOrder = drawTreePartialOrder'
drawGraph' Circle = drawFlatCircle'
drawGraph' Hierarchical = drawHier'
drawGraph' Adaptive = drawFlatAdaptive'

-- | Saves a diagram to an SVG file at the specified 'FilePath', with the specified 'Dimensions'. Only one dimension is needed, they are given in the format:
-- @
-- (Maybe Double, Maybe Double)
-- @
saveSVG :: FilePath -> Dimensions -> Diagram B -> IO ()
saveSVG path (w,h) d = renderSVG path (mkSizeSpec2D w h) d