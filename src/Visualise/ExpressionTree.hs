module Visualise.ExpressionTree (
    drawExpressionTree, drawExpressionTree'
) where

import Visualise.Common
import Algebra.Graph         hiding ((===))
import Diagrams.Prelude      hiding (Empty)
import Diagrams.Backend.SVG

-- | The default diagram for 'Overlay' link nodes - a circle of size '0.1' with a '+' inside.
defaultOverlayDiagram :: Diagram B
defaultOverlayDiagram = (drawDefaultEmptyNode 0.1 <> text "+" # fontSizeL 0.1)

-- | The default diagram for 'Connect' link nodes - a circle of size '0.1' with a '*' inside.
defaultConnectDiagram :: Diagram B
defaultConnectDiagram = (drawDefaultEmptyNode 0.1 <> text "*" # fontSizeL 0.1 # translateY (-0.02))

-- | Takes a left and right diagram and links them using the provided 'linkNode' 'Diagram' and "Diagrams.Prelude"'s 'conenctOutside' for links.
branch :: Diagram B -> (Diagram B, String) -> (Diagram B, String) -> (Diagram B, String)
branch linkNode (a,c1) (b,c2) = (connectOutside' arrowOpts newLink bLink $ connectOutside' arrowOpts newLink aLink diag, newCount)
    where arrowOpts = with & shaftStyle %~ lw thin & arrowHead .~ noHead
          diag = (a # translateY (-0.4) ||| linkNode # named newLink ||| b # translateY (-0.4))
          newCount = c1 ++ c2
          newLink = "link_" ++ newCount
          aLink = "link_" ++ c1
          bLink = "link_" ++ c2

-- | Visualises the provided "Graph" in the form of an expression tree with custom nodes and link nodes.
-- Folds through the graph with tuples of the type ('Diagram B', 'String') where the 'Diagram' is the layout of the graph section and the 'Sting' is an accumulator used for link identification when joining branches.
drawExpressionTree' :: (Show a, Countable a)  -- ^ The graph's vertices must be showable and be able to be counted.
                    => Diagram B              -- ^ The default 'Empty' node 'Diagram'.
                    -> (a -> Diagram B)       -- ^ A function which takes a vertex and produces a node 'Diagram'.
                    -> Diagram B              -- ^ A 'Diagram' used for linking two expression tree branches when "Overlay" is used, by default 'defaultOverlayDiagram' is used (a circle containing a '+').
                    -> Diagram B              -- ^ A 'Diagram' used for linking two expression tree branches when "Connect" is used, by default 'defaultConnectDiagram' is used (a circle containing a '*').
                    -> Graph a                -- ^ The 'Graph' to be visualised as an expression tree.
                    -> Diagram B              -- ^ The resultant expression tree 'Diagram'.
drawExpressionTree' emptyNode drawF overlayDiag connectDiag g = (fst $ foldg ((emptyNode # named "link_e","e")) (\a -> let c = "v" ++ show a in (drawF a # named ("link_" ++ c),c)) (branch overlayDiag) (branch connectDiag) g) # frame 0.1

-- | Visualises the provided "Graph" in the form of an expression tree with default nodes and link nodes, giving a "Diagram" as its output.
drawExpressionTree :: (Show a, Countable a) => Graph a -> Diagram B
drawExpressionTree = drawExpressionTree' (drawDefaultEmptyNode 0.1) drawDefaultNode defaultOverlayDiagram defaultConnectDiagram