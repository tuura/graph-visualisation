-----------------------------------------------------------------------------
-- |
-- Module: Visualise.ExpressionTree
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Visualises an algebraic graph expression as an expression tree: each
-- vertex is a leaf and are joined by a node which is represented by a
-- different <https://hackage.haskell.org/package/diagrams Diagram> depending on if the two vertices are joined by a <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Connect>
-- or an <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Overlay>. By default <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Connect> is represented by a '*' in a circle
-- and <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Overlay> is represented by a '+' in a circle.
-- 
-- 'drawExpressionTree' can be used to draw the graph representation with 
-- default 'Settings' and 'Drawing's. This function can be invoked by the
-- main drawing function 'drawGraph' in the main "Visualise" module, using the
-- 'ExpressionTree' 'Method' constructor.
-- 'drawExpressionTree'' can also be used but as well as a graph extra
-- parameters can be provided to determine how to draw different nodes in the
-- expression tree, however this function cannot be invoked using 'drawGraph''
-- from "Visualise" so must be used directly.
--
-----------------------------------------------------------------------------
module Visualise.ExpressionTree (
    -- * Visualises a graph as an expression tree with default 'Settings'.
    drawExpressionTree, 

    -- * Visualises a graph as an expression tree drawn using the specified parameters.
    drawExpressionTree',

    -- * Provides a default node diagram for overlayed branches.
    defaultOverlayDiagram,

    -- * Provides a default node diagram for connected branches.
    defaultConnectDiagram
) where

import Visualise.Common
import Algebra.Graph         hiding ((===))
import Diagrams.Prelude      hiding (Empty)
import Diagrams.Backend.SVG

-- | The default diagram for <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Overlay> link nodes - a circle of size '0.1' with a '+' inside.
defaultOverlayDiagram :: Diagram B
defaultOverlayDiagram = drawDefaultEmptyNode 0.1 <> text "+" # fontSizeL 0.1

-- | The default diagram for <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Connect> link nodes - a circle of size '0.1' with a '*' inside.
defaultConnectDiagram :: Diagram B
defaultConnectDiagram = drawDefaultEmptyNode 0.1 <> text "*" # fontSizeL 0.1 # translateY (-0.02)

-- | Takes a left and right diagram and links them using the provided 'linkNode' <https://hackage.haskell.org/package/diagrams Diagram> and "Diagrams.Prelude"'s 'conenctOutside' for links.
-- The 'String' tuple elements are accumulators used for the naming of linking nodes.
branch :: Diagram B -> (Diagram B, String) -> (Diagram B, String) -> (Diagram B, String)
branch linkNode (a,c1) (b,c2) = (connectOutside' arrowOpts newLink bLink $ connectOutside' arrowOpts newLink aLink diag, newCount)
    where arrowOpts = with & shaftStyle %~ lw thin & arrowHead .~ noHead
          diag = (a # translateY (-0.4) ||| linkNode # named newLink ||| b # translateY (-0.4))
          newCount = c1 ++ c2
          newLink = "link_" ++ newCount
          aLink = "link_" ++ c1
          bLink = "link_" ++ c2

-- | Visualises the provided "Graph" in the form of an expression tree with custom nodes and link nodes.
-- Folds through the graph with tuples of the type ('Diagram B', 'String') where the <https://hackage.haskell.org/package/diagrams Diagram> is the layout of the graph section and the 'Sting' is an accumulator used for link identification when joining branches.
drawExpressionTree' :: (Show a, Countable a) => Diagram B              -- ^ The default <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Empty> node <https://hackage.haskell.org/package/diagrams Diagram>.
                                             -> (a -> Diagram B)       -- ^ A function which takes a vertex and produces a node <https://hackage.haskell.org/package/diagrams Diagram>.
                                             -> Diagram B              -- ^ A <https://hackage.haskell.org/package/diagrams Diagram> used for linking two expression tree branches when "Overlay" is used, by default 'defaultOverlayDiagram' is used (a circle containing a '+').
                                             -> Diagram B              -- ^ A <https://hackage.haskell.org/package/diagrams Diagram> used for linking two expression tree branches when "Connect" is used, by default 'defaultConnectDiagram' is used (a circle containing a '*').
                                             -> Graph a                -- ^ The graph to be visualised as an expression tree.
                                             -> Diagram B              -- ^ The resultant expression tree <https://hackage.haskell.org/package/diagrams Diagram>.
drawExpressionTree' emptyNode drawF overlayDiag connectDiag g = (fst $ foldg ((emptyNode # named "link_e","e")) (\a -> let c = "v" ++ show a in (drawF a # named ("link_" ++ c),c)) (branch overlayDiag) (branch connectDiag) g) # frame 0.1

-- | Visualises the provided "Graph" in the form of an expression tree with default nodes and link nodes, giving a "Diagram" as its output.
drawExpressionTree :: (Show a, Countable a) => Graph a -> Diagram B
drawExpressionTree = drawExpressionTree' (drawDefaultEmptyNode 0.1) drawDefaultNode defaultOverlayDiagram defaultConnectDiagram