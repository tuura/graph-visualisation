{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Visualise.FlatAdaptive (
    Settings (..),

    drawFlatAdaptive, drawFlatAdaptive'
) where

import Visualise.Common
import Algebra.Graph
import Diagrams.Prelude hiding (Empty, union)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
import Data.List
import Data.Function
import Data.Maybe

-- | Produces a regular polygon of the specified number of sides and gets the points coorrisponding to the polygon's vertices.
layoutVertices :: Int -> [Point V2 Double]
layoutVertices n = trailVertices $ regPoly n 1

-- | Gets all the connections to each node, regardless of direction.
-- Combines the 'ConnectList's from 'connectedTo' and 'connectedFrom' and groups them using 'groupConnected' to give a 'ConnectList' where the second tuple elements are all the incoming and outgoing conenctions for the vertex in the first element of the tuple.
connected :: (Show a, Eq a, Ord a) => [a] -> [(a,a)] -> ConnectList a
connected n l = groupConnected $ sortBy (\(x,_) (y,_) -> x `compare` y) (connectedTo l ++ connectedFrom l)

-- | Groups together elements in the provided 'ConnectList' to stop more than one tuple element with the same first element.
-- Results in a maximum of one tuple element per vertex, with the second tuple element containing the union of tuple elements with the same first element in the original list.
groupConnected :: (Eq a) => ConnectList a -> ConnectList a
groupConnected [] = []
groupConnected [(a,bs),(c,ds)]
    | a == c = [(a,bs `union` ds)]
    | otherwise = (a,bs) : [(c,ds)]
groupConnected ((a,bs) : (c,ds) : es)
    | a == c = groupConnected ((a,bs `union` ds) : es)
    | otherwise = (a,bs) : groupConnected((c,ds) : es)

-- | Sorts the provided 'ConnectList' by the length of the second tuple element lists.
getGroups :: ConnectList a -> ConnectList a
getGroups = sortBy (compare `on` length . snd)

-- | TODO: Carry on Haddock here
layoutGroups :: (Show a, Eq a) => Diagram B -> [a] -> ConnectList a -> Diagram B
layoutGroups d n [] = mempty
layoutGroups d n ((a,bs):xs)
    | null xs = makeGroup d (a:bs) (n \\ (a:bs))
    | otherwise = layoutGroups (makeGroup d (a:bs) (n \\ (a:bs))) n xs

makeGroup :: (Show a, Eq a) => Diagram B -> [a] -> [a] -> Diagram B
makeGroup d c others = withNames (show <$> others) (\otherSubs d -> 
        withNames (show <$> c) (\subs@(s:ss) d -> (mconcat $ getSub <$> otherSubs) <> (atPoints (layoutVertices . length $ subs) $ getSub <$> subs)) d) d
          
drawArrow :: Settings -> String -> String -> Diagram B -> Diagram B
drawArrow s a b d
    | a == b = connectPerim' arrowOpts2 a b (0 @@ turn) (-1/2 @@ turn) d
    | otherwise = connectOutside' arrowOpts1 a b d
    where arrowOpts1 = with & shaftStyle %~ lw (dynamicThick s) & if directed s == Directed then headLength .~ dynamicHead s else arrowHead .~ noHead
          arrowOpts2 = with & shaftStyle %~ lw (dynamicThick s) & arrowShaft .~ arc xDir (4/6 @@ turn) & if directed s == Directed then headLength .~ dynamicHead s else arrowHead .~ noHead

initialPositions :: (Draw a) => Int -> [a] -> Diagram B
initialPositions 1 n = cat' (r2 (-1,1)) (with & catMethod .~ Distrib & sep .~ 0.5) $ draw <$> n
initialPositions 2 n = hsep 0.3 $ draw <$> n

connectedOnlyDiagram :: [Node] -> [(Node,Node)] -> Diagram B -> Diagram B
connectedOnlyDiagram nodes connections initialDiagram = layoutGroups initialDiagram nodes $ getGroups (connected nodes connections)

overlayedOnlyDiagram :: (Draw a, Eq a) => [a] -> [a] -> Diagram B
overlayedOnlyDiagram nodes connectedOnlyList = hsep 0.2 $ draw <$> (nodes \\ connectedOnlyList)

listConnectedOnly :: (Eq a) => [(a,a)] -> [a]
listConnectedOnly connections = nub $ foldr (\(a,bs) acc -> a : bs ++ acc) [] (connectedTo connections ++ connectedFrom connections)

drawFlatAdaptive :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawFlatAdaptive = drawFlatAdaptive' defaultAdaptiveSettings drawDefaultNode

drawFlatAdaptive' :: (Show a, Eq a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawFlatAdaptive' settingsF drawF g = outDiag <> boundingRect outDiag
    where outDiag = (foldr (\(a,b) acc -> drawArrow s (name a) (name b) acc) beforeArrowsDiag connections) # frame 0.1
          beforeArrowsDiag = overlayedDiagram ||| strutX 0.1 ||| connectedDiagram
          overlayedDiagram = overlayedOnlyDiagram nodes connectedNodes
          connectedDiagram = connectedOnlyDiagram connectedNodes connections . initialPositions (fromJust . initPos $ s) $ connectedNodes
          connectedNodes = listConnectedOnly connections
          (ProcessedGraph nodes connections) = getVertices drawF g
          s = settingsF g

-- drawFlatAdaptive :: (Show a) => FilePath -> Dimensions -> Graph a -> Diagram B
-- drawFlatAdaptive path dims g = drawFlatAdaptive' (defaultSettings g) path dims g

-- drawFlatAdaptive' :: (Show a) => Settings -> FilePath -> Dimensions -> Diagram B
-- drawFlatAdaptive' s path dims g = draw path dims $ visualiseFlatAdaptive s g # frame 0.1


defaultAdaptiveSettings :: (Countable a) => Graph a -> Settings
defaultAdaptiveSettings g = Settings (dynamicStyle small $ count g) 
                             (dynamicStyle thin $ count g)
                             Directed
                             Nothing
                             Nothing
                             Nothing
                             Nothing
                             Nothing
                             (Just 1)

-- main = draw "../tests/test_sep.svg" (Just 1000,Nothing) $ drawFlatAdaptive (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))


-- main = mainWith $ visualiseFlatAdaptive defaultSettings (show <$> inputTestData) # frame 0.1
-- main = mainWith $ visualiseFlatAdaptive () inputTestData # frame 0.1

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
-- inputTestData = (Connect (Vertex 2) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))
inputTestData = (Overlay (Vertex 50) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))
