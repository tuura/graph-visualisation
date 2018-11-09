-----------------------------------------------------------------------------
-- |
-- Module: Visualise
-- Description : Provides a series of functions for drawing algebraic graphs.
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Provides two graph drawing functions - 'drawGraph' and 'drawGraph'' - that
-- can invoke various graph drawing functions depending on their 'Method' parameter.
-- 'drawGraph' draws a graph with default settings whereas 'drawGraph'' can be
-- provided with a 'Settings' parameter. The graph to be drawn is an instance
-- of the type graph as defined by "Algebra.Graph".
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

    -- * The type used to represent the dimensions of an output <https://hackage.haskell.org/package/diagrams Diagram>, a tuple of Maybe Doubles.
    Dimensions,

    -- * The two main graph drawing functions.
    drawGraph, drawGraph',

    -- * The function for drawing with GraphViz, reexported from "Visualise.GraphViz".
    drawWithGraphViz,

    -- * The functions for drawing an expression tree from a graph reexported from "Visualise.ExpressionTree". 
    -- 'drawExpressionTree' can be used with 'drawGraph' and the 'Method' constructor but 'drawExpressionTree'' can't as it requires extra parameters.
    drawExpressionTree, drawExpressionTree',

    -- * Saves a <https://hackage.haskell.org/package/diagrams Diagram> to to a specified SVG file with the specified 'Dimensions'.
    saveSVG
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

-- | The 'Dimensions' data type is used to store the dimensions for writing a <https://hackage.haskell.org/package/diagrams Diagram> to a file. Either the width or height (or both) can be provided in a maybe tuple in the order (Width, Height).
type Dimensions = (Maybe Double, Maybe Double)

-- | Draw a graph using the specified 'Method', uses the default 'Settings' for the specified drawing 'Method'.
-- To draw a graph using "Visualise.GraphViz" or "Visualise.ExpressionTree", their own drawing functions must be used: for "Visualise.GraphViz" the function 'drawWithGraphViz' and for "Visualise.ExpressionTree" the function 'drawExpressionTree' or 'drawExpressionTree''.
drawGraph :: (Show a, Eq a, Countable a) => Method -> Graph a -> Diagram B
drawGraph Tree = drawTree
drawGraph TreePartialOrder = drawTreePartialOrder
drawGraph Circle = drawFlatCircle
drawGraph Hierarchical = drawHier
drawGraph Adaptive = drawFlatAdaptive
drawGraph ExpressionTree = drawExpressionTree

-- | Draw a graph using the specified 'Method' and 'Settings'.
-- Using Lens setters the default 'Settings' for each graph-drawing method can be customised like so:
--
-- @
-- ghci> g = Connect (Vertex 1) (Vertex 2) :: Graph Int
-- ghci> s = (defaultTreeSettings g) & directed .~ Undirected
-- ghci> d = drawGraph' Tree s g
-- ghci> saveSVG test_drawing.svg (Just 1000, Nothing) d
-- @
--
-- This creates a graph of integer vertices, creates a 'Settings' instance from the default tree drawing 'Settings' but modifies the '_directed' field using the Lens setter function to make the graph undirected. 
-- Then this 'drawGraph'' function is used to draw the graph using the settings, which is then output to an svg file using the function 'saveSVG'.
drawGraph' :: (Show a, Eq a, Countable a) => Method -> Settings a -> Graph a -> Diagram B
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