module Visualise.ExpressionTree (
    visualiseExpressionTree, drawExpressionTree, drawExpressionTree', defaultExpressionTreeSettings
) where

import Visualise.Common
import Algebra.Graph        hiding ((===))
import Diagrams.Prelude
import Diagrams.Backend.SVG

data BranchType = C | O deriving (Eq)

branch :: Settings -> BranchType -> Diagram B -> Diagram B -> Diagram B
branch s brType a b = localize (connectOutside' arrowOpts "link" "b" $ connectOutside' arrowOpts "link" "a" (a # named "a" # translateY (-0.5) ||| (getLinkNode brType) # named "link" ||| b # named "b" # translateY (-0.5)) # centerX)
    where arrowOpts = with & shaftStyle %~ lw (dynamicThick s) & if directed s == Directed then headLength .~ dynamicHead s else arrowHead .~ noHead

getLinkNode :: BranchType -> Diagram B
getLinkNode brType
    | brType == C = (circle 0.1 <> text "*" # fontSizeL 0.1 # translateY (-0.02))
    | brType == O = (circle 0.1 <> text "+" # fontSizeL 0.1)


visualiseExpressionTree :: (Show a) => Settings -> (a -> Diagram B) -> Graph a -> Diagram B
visualiseExpressionTree s drawF = foldg (circle 0.1) (drawF) (branch s O) (branch s C)

drawExpressionTree :: (Show a, Countable a) => Graph a -> Diagram B
drawExpressionTree = drawExpressionTree' defaultExpressionTreeSettings drawDefaultNode

drawExpressionTree' :: (Show a, Countable a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawExpressionTree' settingsF drawF g = visualiseExpressionTree (settingsF g) drawF g # frame 0.1

defaultExpressionTreeSettings :: (Countable a) => Graph a -> Settings
defaultExpressionTreeSettings g = Settings (dynamicStyle small $ count g) 
                                           (dynamicStyle thin $ count g) 
                                           Undirected 
                                           Nothing
                                           Nothing
                                           Nothing
                                           Nothing 
                                           Nothing 
                                           Nothing