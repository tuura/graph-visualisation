{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-----------------------------------------------------------------------------
-- |
-- Module: Visualise.Hierarchical
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Draws a graph as a hierarchical graph by grouping together vertices with
-- common connections and containing them with a box.
--
-- The main drawing function 'drawHier' draws a hierarchical graph with default
-- 'Settings' as defined by 'defaultHierSettings' with alternating background
-- colours as defined by 'alternatingColour', as well as a second function
-- 'drawHier'' which produces a customised drawing.
--
-----------------------------------------------------------------------------
module Visualise.Hierarchical (
    -- * The hierarchical graph-drawing functions.
    drawHier, drawHier', 

    -- * The default 'Settings' used by 'drawHier'.
    defaultHierSettings, 

    -- * The default node-drawing function used by 'defaultHierSettings'.
    drawDefaultHierNode,

    -- * The alternating-colour-producing function using by 'defaultHierSettings'.
    alternatingColour
) where

import Algebra.Graph        hiding ((===))
import Visualise.Common     hiding (name)
import Diagrams.Prelude     hiding (Empty)
import Diagrams.Backend.SVG
import Data.Maybe

-- | The default function to produce a <https://hackage.haskell.org/package/diagrams Diagram> for a vertex.
-- Calls 'show' on the provided value and puts the text in a circle.
drawDefaultHierNode :: (Show a) => a -> Diagram B 
drawDefaultHierNode nn = text n # href ("javascript:alert(\"Node " ++ n ++ "\")") # fontSizeL 0.4 <> circle 0.7 # lwL 0.05 # named n
    where n = show nn

-- | Parses the provided graph using recursion and draws each vertex using the provided vertex-drawing function and 'Settings'. 
-- When <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Overlay> is used the two graphs are drawn next to each other vertically surrounded by a coloured box, where the colour is determined by the 'Settings'. 
-- For <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Connect> the graphs are drawn next to each other horizontally, surrounded by a box and also connected with an arrow.
visualiseHier :: (Show a) => Graph a           -- ^ The Graph to be drawn
                          -> Int               -- ^ The current depth the recursion is into the graph, function should be called with 0 for this value.
                          -> Settings a        -- ^ The 'Settings' for the visualisation.
                          -> Diagram B         -- ^ The output <https://hackage.haskell.org/package/diagrams Diagram> for the graph.
visualiseHier g@(Vertex a) l s = (s ^. nodeDrawFunction $ a) # lwL 0.05
visualiseHier g@(Overlay g1 g2) l s = (drawn <> boundingRect drawn # fc ((fromJust $ s ^. colF) l) # lw none # opacity (fromJust $ s ^. bgOp)) # named (show g)
    where drawn = (visualiseHier g1 (l + 1) s === strutY 1 === visualiseHier g2 (l + 1) s) # frame 0.2
visualiseHier g@(Connect g1 g2) l s = (arrowed <> boundingRect arrowed # fc ((fromJust $ s ^. colF) l) # lw none # opacity (fromJust $ s ^. bgOp)) # named (show g)
    where arrowed = connectOutside' arrowOpts (show g1) (show g2) drawn
          drawn = (visualiseHier g1 (l + 1) s ||| strutX 1 ||| visualiseHier g2 (l + 1) s) # frame 0.2
          arrowOpts = with & shaftStyle %~ lw (s ^. dynamicThick) & if (s ^. directed) == Directed then headLength .~ (s ^. dynamicHead) else arrowHead .~ noHead

-- | The default drawing function which uses 'drawHier'' with the default 'Settings' provided by the function 'defaultHierSettings'. 
-- The background colour for each layer alternates using the 'alternatingColour' function and have the opacity of '1'.
drawHier :: (Show a, Countable a) => Graph a -> Diagram B
drawHier g = drawHier' (defaultHierSettings g) g

-- | Takes 'Settings', a vertex-to-<https://hackage.haskell.org/package/diagrams Diagram> function and a graph to produce a hierarchical representation for the graph in the form of a <https://hackage.haskell.org/package/diagrams Diagram>.
drawHier' :: (Show a, Countable a) => Settings a -> Graph a -> Diagram B
drawHier' s g = visualiseHier g 0 s # frame 0.1

-- | Produces the default 'Settings' for the supplied graph. 
-- The arrow heads and shafts automatically scale to the number of graph vertices, the graph is 'Directed' and the group background colours alternate using 'alternatingColour' by default with an opacity of 1. 
defaultHierSettings :: (Countable a, Show a) => Graph a -> Settings a
defaultHierSettings g = (with :: (Show a) => Settings a) & (nodeDrawFunction .~ drawDefaultHierNode :: (Show a) => Settings a -> Settings a)
                                                         & dynamicHead .~ (dynamicStyle normal $ count g) 
                                                         & dynamicThick .~ (dynamicStyle thin $ count g)
                                                         & colF .~ Just alternatingColour
                                                         & bgOp .~ Just 1


-- | If the current depth is odd the colour is red and if it's even then the colour is cyan.
alternatingColour :: Int            -- ^ The depth of the current layer of the graph.
                  -> Colour Double  -- ^ The background colour for the layer.
alternatingColour i
    | odd i = red
    | otherwise = cyan

