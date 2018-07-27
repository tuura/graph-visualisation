module Visualise.ExpressionTree (
    drawExpressionTree, drawExpressionTree'
) where

import Visualise.Common
import Algebra.Graph         hiding ((===))
import Diagrams.Prelude
import Diagrams.Backend.SVG

-- | The default diagram for 'Overlay' link nodes - a circle of size '0.1' with a '+' inside.
defaultOverlayDiagram :: Diagram B
defaultOverlayDiagram = (circle 0.1 <> text "+" # fontSizeL 0.1)

-- | The default diagram for 'Connect' link nodes - a circle of size '0.1' with a '*' inside.
defaultConnectDiagram :: Diagram B
defaultConnectDiagram = (circle 0.1 <> text "*" # fontSizeL 0.1 # translateY (-0.02))

-- | The default diagram for 'Empty' graph nodes - a blank circle of size '0.1'.
drawDefaultEmptyNode :: Diagram B
drawDefaultEmptyNode = circle 0.1

-- | Takes a left and right diagram and links them using the provided 'linkNode' 'Diagram' and "Diagrams.Prelude"'s 'conenctOutside' for links.
branch :: Diagram B -> Diagram B -> Diagram B -> Diagram B
branch linkNode a b = localize (connectOutside' arrowOpts "link" "b" $ connectOutside' arrowOpts "link" "a" (a # named "a" # translateY (-0.4) ||| linkNode # named "link" ||| b # named "b" # translateY (-0.4)) # centerX)
    where arrowOpts = with & shaftStyle %~ lw thin & arrowHead .~ noHead

-- | Visualises the provided "Graph" in the form of an expression tree with custom nodes and link nodes.
-- Folds through the graph
drawExpressionTree' :: (Show a, Countable a) => Diagram B         -- ^ The default 'Empty' node 'Diagram'.
                                             ->(a -> Diagram B)   -- ^ A function which takes a vertex and produces a node 'Diagram'.
                                             -> Diagram B         -- ^ A 'Diagram' used for linking two expression tree branches when "Overlay" is used, by default 'defaultOverlayDiagram' is used (a circle containing a '+').
                                             -> Diagram B         -- ^ A 'Diagram' used for linking two expression tree branches when "Connect" is used, by default 'defaultConnectDiagram' is used (a circle containing a '*').
                                             -> Graph a           -- ^ The 'Graph' to be visualised as an expression tree.
                                             -> Diagram B         -- ^ The resultant expression tree 'Diagram'.
drawExpressionTree' emptyNode drawF overlayDiag connectDiag g = (foldg (emptyNode) (drawF) (branch overlayDiag) (branch connectDiag) g) # frame 0.1

-- | Visualises the provided "Graph" in the form of an expression tree with default nodes and link nodes, giving a "Diagram" as its output.
drawExpressionTree :: (Show a, Countable a) => Graph a -> Diagram B
drawExpressionTree = drawExpressionTree' drawDefaultEmptyNode drawDefaultNode defaultOverlayDiagram defaultConnectDiagram