{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module: Visualise.FlatAdaptive
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Draws a graph in an adaptive way by grouping together connected vertices.
-- Works with some graphs however currently needs work.
-- 
-- Provides two drawing functions: 'drawFlatAdaptive' which uses default
-- 'Settings' and 'drawFlatAdaptive'' which can be customised.
--
-----------------------------------------------------------------------------
module Visualise.FlatAdaptive (
    -- * Draws a graph with an adaptive layout with default 'Settings'.
    drawFlatAdaptive, 

    -- * Draws a graph with an adaptive layout with customised 'Settings'.
    drawFlatAdaptive', 

    -- * The default 'Settings' function used by 'drawFlatAdaptive'.
    defaultAdaptiveSettings
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

-- | Produces a regular polygon of the specified number of sides and gets the points corrisponding to the polygon's vertices.
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

-- | Goes through the groups contained in the supplied 'ConnectList', using recursion to apply 'makeGroup' to each group.
layoutGroups :: (Show a, Eq a)   
             => Diagram B       -- ^ The initial ungrouped <https://hackage.haskell.org/package/diagrams Diagram>.
             -> [a]             -- ^ The list of vertex names.
             -> ConnectList a   -- ^ The list of connection groups
             -> Diagram B       -- ^ The resultant <https://hackage.haskell.org/package/diagrams Diagram> with groups.
layoutGroups d n [] = mempty
layoutGroups d n ((a,bs):xs)
    | null xs = makeGroup d (a:bs) (n \\ (a:bs))
    | otherwise = layoutGroups (makeGroup d (a:bs) (n \\ (a:bs))) n xs

-- | Groups together the vertices with the requested names at the vertices of a polygon produced by 'layoutVertices', using 'withNames' from "Diagrams.Prelude" to get the subdiagrams corrisponding to the supplied names.
makeGroup :: (Show a, Eq a)  
          => Diagram B       -- ^ The current diagram before the current grouping takes place.
          -> [a]             -- ^ The list of vertex names to be grouped together.
          -> [a]             -- ^ The list of the other vertex names not to be grouped.
          -> Diagram B       -- ^ The diagram with the grouping complete.
makeGroup d c others = withNames (show <$> others) (\otherSubs d -> withNames (show <$> c) (grouping otherSubs) d) d
    where grouping otherSubs subs d = (mconcat $ getSub <$> otherSubs) <> (atPoints (layoutVertices . length $ subs) $ getSub <$> subs)
          
-- | Connects the two specified vertices with an arrow. If the two vertex names are the same then a self-loop is drawn with alternate settings.
-- The 'Settings' provided determine the properties of the connections, e.g. shaft thickness, arrow head size or even if there is an arrow head at all (there won't be if the graph is 'Undirected').
drawArrow :: (Show b, Eq b) => Settings a -> b -> b -> Diagram B -> Diagram B
drawArrow s a b d
    | a == b = connectPerim' arrowOpts2 (show a) (show b) (0 @@ turn) (-1/2 @@ turn) d
    | otherwise = connectOutside' arrowOpts1 (show a) (show b) d
    where arrowOpts1 = with & shaftStyle %~ lw (s ^. dynamicThick) & if (s ^. directed) == Directed then headLength .~ (s ^. dynamicHead) else arrowHead .~ noHead
          arrowOpts2 = with & shaftStyle %~ lw (s ^. dynamicThick) & arrowShaft .~ arc xDir (4/6 @@ turn) & if (s ^. directed) == Directed then headLength .~ (s ^. dynamicHead) else arrowHead .~ noHead

-- | Produces a <https://hackage.haskell.org/package/diagrams Diagram> of all the graph's vertices to be used as a basis for grouping together vertices. Has variations that can be chosen with the 'initPos' in the 'Settings' used.
initialPositions :: (Draw a) => Int        -- ^ Corrisponds to which intial layout function will be used, best to use trial-and-error on a per-graph basis.
                             -> [a]        -- ^ The list of vertices.
                             -> Diagram B  -- ^ The resultant initial <https://hackage.haskell.org/package/diagrams Diagram>.
initialPositions 1 n = cat' (r2 (-1,1)) (with & catMethod .~ Distrib & sep .~ 0.5) $ draw <$> n
initialPositions 2 n = hsep 0.3 $ draw <$> n

-- | Draws the grouped graph vertices which have connections, vertices with only overlays are not included.
connectedOnlyDiagram :: (Show a, Ord a) 
                     => [a]             -- ^ A list of the connected vertices.
                     -> [(a,a)]         -- ^ A list of the vertex connections.
                     -> Diagram B       -- ^ The intial diagram without grouping.
                     -> Diagram B       -- ^ The resultant diagram with grouped vertices.
connectedOnlyDiagram nodes connections initialDiagram = layoutGroups initialDiagram nodes $ getGroups (connected nodes connections)

-- | Draws the rest of the vertices not drawn by 'connectedOnlyDiagram', separated horizontally by '0.2'.
overlayedOnlyDiagram :: (Draw a, Eq a) => [a] -> Diagram B
overlayedOnlyDiagram overlayedOnlyNodes = hsep 0.2 $ draw <$> overlayedOnlyNodes

-- | Takes a list of tuples representing a graph's connections and returns all the elements present in the list (in either element of the tuples).
listConnectedOnly :: (Eq a) => [(a,a)] -> [a]
listConnectedOnly = nub . foldr (\(a,b) acc -> a : b : acc) []

-- | Uses 'drawFlatAdaptive'' with default 'Settings' provided by 'defaultAdaptiveSettings'.
drawFlatAdaptive :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawFlatAdaptive g = drawFlatAdaptive' (defaultAdaptiveSettings g) g

-- | Draws a flat graph with an adaptive layout.
drawFlatAdaptive' :: (Show a, Eq a) => Settings a -> Graph a -> Diagram B
drawFlatAdaptive' s g = outDiag <> boundingRect outDiag
    where outDiag = (foldr (\(a,b) acc -> drawArrow s a b acc) beforeArrowsDiag connections) # frame 0.1
          beforeArrowsDiag = overlayedDiagram ||| strutX 0.1 ||| connectedDiagram
          overlayedDiagram = overlayedOnlyDiagram (nodes \\ connectedNodes)
          connectedDiagram = connectedOnlyDiagram connectedNodes connections . initialPositions (fromJust $ s ^. initPos) $ connectedNodes
          connectedNodes = listConnectedOnly connections
          (ProcessedGraph nodes connections) = getVertices (s ^. nodeDrawFunction) g


-- | The default 'Settings' function for graphs generated with "Visualise.FlatAdaptive".
defaultAdaptiveSettings :: (Countable a, Show a) => Graph a -> Settings a
defaultAdaptiveSettings g = with & dynamicHead .~ (dynamicStyle small $ count g) 
                                 & dynamicThick .~ (dynamicStyle thin $ count g)
                                 & initPos .~ Just 1
