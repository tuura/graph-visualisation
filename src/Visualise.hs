-----------------------------------------------------------------------------
-- |
-- Module: Visualise
-- Description : Provides a series of functions for drawing algebraic graphs.
-- Copyright : (c) Sam Prescott 2018
-- 
-- Provides two graph drawing functions - 'drawGraph' and 'drawGraph'' - that
-- can invoke various graph drawing functions depending on their 'Method' parameter.
-- 'drawGraph' draws a graph with default settings whereas 'drawGraph'' can be
-- provided with a 'Settings' parameter. The 'Graph' to be drawn is an instance
-- of the type 'Graph' as defined by "Algebra.Graph".
-- 
-- The five graph drawing 'Method's defined for 'Method' can be used for both
-- drawing functions, apart from 'ExpressionTree' which can only be used for
-- 'drawGraph' not 'drawGraph'' due to its advanced parameters. Therefore
-- 'drawExpressionTree' and 'drawExpressionTree'' are reexported from
-- "Visualise.ExpressionTree".
--
-- To draw a graph using "Data.GraphViz" the function 'drawWithGraphViz' from
-- "Visualise.GraphViz" can be used.
--
-----------------------------------------------------------------------------
module Visualise (
    -- * The constructors for the graph-drawing methods.
    Method(..),

    -- * The two main graph drawing functions.
    drawGraph, drawGraph', saveSVG,

    -- * The function for drawing with GraphViz, reexported from "Visualise.GraphViz".
    drawWithGraphViz,

    -- * The functions for drawing an expression tree from a graph reexported from "Visualise.ExpressionTree". 
    -- 'drawExpressionTree' can be used with 'drawGraph' and the 'Method' constructor but 'drawExpressionTree'' can't as it requires extra parameters.
    drawExpressionTree, drawExpressionTree'

) where

import Visualise.Common
import Visualise.Tree
import Visualise.FlatCircle
import Visualise.Hierarchical
import Visualise.FlatAdaptive
import Visualise.GraphViz
import Visualise.ExpressionTree
import Algebra.Graph
import Diagrams.Prelude          hiding (Empty)
import Diagrams.Backend.SVG
import Data.GraphViz.Commands

-- | The graph drawing method
data Method = Tree              -- ^ The 'drawTree' function from "Visualise.Tree" module will be used to draw the graph.
            | TreePartialOrder  -- ^ The 'drawTreePartialOrder' function "Visualise.Tree" module will be used to draw the graph, with all indirect dependencies removed by using the Coffman-Graham algorithm.
            | Circle            -- ^ The "Visualise.FlatCircle" module will be used to draw the graph, with the verticies being placed on the vertecies of a regular polygon of n sides (where n is the number of graph vertices).
            | Hierarchical      -- ^ The "Visualise.Hierarchical" module will be used to draw the graph as a hierarchical graph by grouping together vertices and groups of vertices with common connections.
            | Adaptive          -- ^ The "Visualise.FlatAdaptive" module will be used to draw the graph, with the layout of vertices being dynamic depending on the connections between them.
            | ExpressionTree    -- ^ The "Visualise.ExpressionTree" module will be used to draw an expression tree representation of the graph.

-- | The 'Dimensions' data type is used to store the dimensions for writing a 'Diagram' to a file. Either the width or height (or both) can be provided in a maybe tuple in the order (Width, Height).
type Dimensions = (Maybe Double, Maybe Double)

-- | Draw a graph using the specified method, uses default settings.
-- To draw a graph using "Visualise.GraphViz" or "Visualise.ExpressionTree", their own drawing functions must be used: for "Visualise.GraphViz" the function 'drawWithGraphViz' and for "Visualise.ExpressionTree" the function 'drawExpressionTree' or 'drawExpressionTree''.
drawGraph :: (Show a, Eq a, Countable a) => Method -> Graph a -> Diagram B
drawGraph Tree = drawTree
drawGraph TreePartialOrder = drawTreePartialOrder
drawGraph Circle = drawFlatCircle
drawGraph Hierarchical = drawHier
drawGraph Adaptive = drawFlatAdaptive
drawGraph ExpressionTree = drawExpressionTree

-- | Draw a graph using the specified method, but also using the specified settings (provided as a function that will take a graph and return a 'Settings' instance, see "Visualise").
drawGraph' :: (Show a, Eq a, Countable a) => Method -> (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawGraph' Tree = drawTree'
drawGraph' TreePartialOrder = drawTreePartialOrder'
drawGraph' Circle = drawFlatCircle'
drawGraph' Hierarchical = drawHier'
drawGraph' Adaptive = drawFlatAdaptive'
-- drawGraph' ExpressionTree = drawExpressionTree'

-- | Saves a diagram to an SVG file at the specified 'FilePath', with the specified 'Dimensions'. Only one dimension is needed, they are given in the format:
-- @
-- (Maybe Double, Maybe Double)
-- @
saveSVG :: FilePath -> Dimensions -> Diagram B -> IO ()
saveSVG path (w,h) d = renderSVG path (mkSizeSpec2D w h) d