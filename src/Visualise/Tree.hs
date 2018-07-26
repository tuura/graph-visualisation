{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Visualise.Tree (
    drawTree, drawTree', drawTreePartialOrder, drawTreePartialOrder'
) where

import Visualise.Common
import Algebra.Graph
import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG
import Diagrams.Path
import Data.List
import Data.Maybe

-- | The 'LayerPosition' data type provides three type constructors, which can be used to determine if a 'Node' is on the left, middle or right of a layer.
data LayerPosition = LayerLeft | LayerMiddle | LayerRight deriving (Eq)

-- | Removes indirect dependancies from a graph represented as an adjacency list (as a 'ConnectList') using the Coffman-Graham algorithm, thus simplifying the graph. This can only be done on partial order graphs.
-- Recursively goes through the 'ConnectList' folding through the list of connected vertices for each vertex, each time folding through the rest of the 'ConnectList'.
-- If the first element of a tuple matches this current vertex then any elements in the list contained in the second element of the matched tuple are removed from the list currently being folded through.
reduction :: (Eq a) => ConnectList a  -- ^ The graph in adjacency list form represented as a 'ConnectList'
                    -> ConnectList a  -- ^ Used internally for recursion - the first part of the list that still needs to be checked through
                    -> ConnectList a  -- ^ The resultant reduced adjacency list
reduction [] _ = []
reduction ((x,ys):zs) extras = new : reduction zs (new : extras)
    where new = (x,nub $ foldr check ys ys)
          check y accOuter = foldr (\(a,bs) acc -> if y == a then acc \\ bs else acc) accOuter toCheck
          toCheck = extras ++ zs

-- | Finds the vertices that don't depend on any other vertices - i.e. the roots of the graph.
-- Folds through the graph's adjacency list and deletes 
getRoots :: (Eq a) => [a] -> ConnectList a -> [a]
getRoots = foldr (\(x,ys) acc -> delete x acc)

-- | Topological sorting of a graph represented by a list of vertices and a 'ConnectList' of vertex connections.
-- It sorts the graph by using Kahn's algorithm, producing an ordered list of vertices.
getLevelList :: (Eq a) => [a]            -- ^ The initial list of vertices that roots in the graph
                       -> ConnectList a  -- ^ The adjacency list for the graph
                       -> [a]            -- ^ The topologically ordered list, or partial list as 'getLevelList' is recursive.
getLevelList [] _ = []
getLevelList q firstCTo = let (queue,list,cTo) = foldThroughRoots q firstCTo
                          in list ++ getLevelList queue cTo
          
-- | Folds through the current list of roots (vertices with no incoming conenctions) and folds through each root's connections to generate part of the sorted list.
-- Adds the current root to the sorted list, then folds over its nodes using 'foldThroughConnectedNodes' and adds any nodes which no longer have other connections to the list.
foldThroughRoots :: (Eq a) => [a]                        -- ^ The current queue/roots.
                           -> ConnectList a              -- ^ The current adjacency list -- modified by previous executions of 'foldThroughRoots' to update the removed connections.
                           -> ([a], [a], ConnectList a)  -- ^ The first element is the new queue/new roots, the second element is the updated sorted list and the third element is the new adjacency list.
foldThroughRoots queue firstCTo = foldr (\root (accQueue,accList,accCTo) -> 
                                    let newList = (root : accList)
                                        folded = foldThroughConnectedNodes root accQueue accCTo
                                    in (fst folded, newList, snd folded))
                                  ([],[],firstCTo) queue

-- | Folds through a list of vertices (used by 'foldThroughRoots' for vertices connected to a root).
-- Checks if the current vertex has any other connections other thaan the root. If it doesn't it is added to the sorted list. 
-- Regardless of its connections the connection between it and the root is removed from the adjacency list.
foldThroughConnectedNodes :: (Eq a) => a                     -- ^ The current root from the queue.
                                    -> [a]                   -- ^ The current queue.
                                    -> ConnectList a         -- ^ The current adjacency list.
                                    -> ([a], ConnectList a)  -- ^ The first element is the new queue and the second element is the updated adjacency list.
foldThroughConnectedNodes root queue cTo = foldl (\(accQueue,acc2) b -> -- b is the current node connected to root Node, acc1 is the current list, acc2 is the current adjacency list
                                              if length (getEdgesTo b acc2) <= 1 then (accQueue ++ [b], deleteConnection root b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
                                              else (accQueue, deleteConnection root b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
                                              (queue,cTo) (getEdgesFrom root cTo)

-- | Removes a connection that goes from 'x' to 'r' in the given adjacency list 
deleteConnection :: (Eq a) => a -> a -> ConnectList a -> ConnectList a
deleteConnection x r cTo = (\(a,bs) -> if a == r then (a,delete x bs) else (a,bs)) <$> cTo

-- | Returns whether vertex 'a' depends on vertex 'b' by seeing if 'b' is an element in the list of edges to 'a' from 'getEdgesTo'.
dependsOn :: (Eq a) => a -> a -> ConnectList a -> Bool
dependsOn a b cTo = b `elem` getEdgesTo a cTo

-- | Returns a list of edges to vertex 'a' using the provided adjacency list and the function 'getEdgesToTuple'.
getEdgesTo :: (Eq a) => a -> ConnectList a -> [a]
getEdgesTo x cTo = if isJust $ getEdgesToTuple x cTo then let (Just tup) = getEdgesToTuple x cTo in snd tup else []

-- | Gets the element tuple from the given adjacency list as a 'Maybe' value, where the first element in the tuple is the given vertex.
getEdgesToTuple :: (Eq a) => a -> ConnectList a -> Maybe (a,[a])
getEdgesToTuple x = find (\(a,bs) -> a == x)

-- | Gives a list of vertices that the provided vertex 
-- Folds over the provided adjacency list, checking if the list in the second element of each tuple contains the provided vertex and if so prepends the corrisponding verex in the first element to the list of vertices to be returned.
getEdgesFrom :: (Eq a) => a -> ConnectList a -> [a]
getEdgesFrom x = foldr (\(a,bs) acc -> if x `elem` bs then a : acc else acc) []

-- | Takes a topologically sorted list and an adjacency list and produces a list of lists with each inner list corrisponding to the vertices in a layer.
getLevels :: (Eq a) => [a]            -- ^ The topologically sorted list
                    -> [a]            -- ^ Used for recursion -- the last level generated by the function
                    -> ConnectList a  -- ^ The adjacency list
                    -> [[a]]          -- ^ The list of layers
getLevels [] _ _ = []
getLevels l lastLevel cTo = toFold : getLevels (l \\ toFold) toFold cTo
    where toFold = foldLevel l lastLevel cTo

-- | Folds through the sorted list to produce the next sorted level of the graph.
-- For each vertex it checks if 'moveToNext' returns true, if so it skips the rest of the list and returns the new level. 'nodePosition' is used to determine where on the level to insert a vertex based on connections to the previous layer.
foldLevel :: (Eq a) => [a] -> [a] -> ConnectList a -> [a]
foldLevel l lastLevel cTo = let (left,right,_) = foldr (\x (accL,accR,flag) -> if flag || moveToNext x cTo accL accR then (accL,accR,True) 
                                                                               else case nodePosition x lastLevel cTo of LayerMiddle -> (accL ++ [x], accR, False)
                                                                                                                         LayerRight  -> (accL, x : accR, False)
                                                                                                                         LayerLeft   -> (x:accL, accR, False)
                                                 ) ([],[],False) (reverse l)
                            in left ++ (reverse right)

-- | Takes a vertex and a list of vertices and an adjacency list, sees if the vertex depends on any vertices in the list and if so gives the position (left, middle or right using 'LayerPosition') of the vertex it is dependant on.
nodePosition :: (Eq a) => a -> [a] -> ConnectList a -> LayerPosition
nodePosition x level cTo = foldr (\a acc -> if (x `dependsOn` a) cTo then a `positionInGivenLayer` level else acc) LayerMiddle level

-- | A boolean function which returns 'True' if the end of the currently being generated layer has been reached.
-- Checks if the current vertex depends on any of the vertices in the current layer and if any of the vertices in the layer depend on the current vertex, if so the end of the layer has been reached so 'True' is returned.
moveToNext :: (Eq a) => a -> ConnectList a -> [a] -> [a] -> Bool
moveToNext x cTo accL accR = or ((\a -> dependsOn x a cTo) <$> accL) || or ((\a -> dependsOn x a cTo) <$> accR) || any (\a -> dependsOn a x cTo) accL || any (\a -> dependsOn a x cTo) accR

-- | Produces a list of tuples corrisponding to connections from a given adjacency list.
reducedConnections :: ConnectList a -> [(a,a)]
reducedConnections = foldr (\(x,ys) acc -> ((\y -> (x,y)) <$> ys) ++ acc) []

positionInGivenLayer :: (Eq a) => (a) -> [a] -> LayerPosition
positionInGivenLayer x ys
    | position == midPoint = LayerMiddle
    | position < midPoint = LayerLeft
    | otherwise = LayerRight
        where midPoint = fromIntegral (length ys - 1)/2
              position = fromIntegral (getElemIndex $ x `elemIndex` ys)
              getElemIndex (Just i) = i
              getElemIndex Nothing = 0

visualiseLayers :: (Draw a) => Settings -> [[a]] -> Diagram B
-- visualiseLayers :: _
visualiseLayers s levelled = vsep (fromJust . layerSpacing $ s) $ foldl (\acc level -> center (hsep (fromJust . nodeSpacing $ s) $ 
                                                                                       -- (draw 0.1 0.1 (drawDAG' (\_ -> s))) <$> level
                                                                                       draw <$> level
                                                                                 ) : acc)
                                                     [] levelled

connectNodes :: (Show a, Draw a) => Settings -> ArrowOpts Double -> a -> a -> [[a]] -> Maybe LayerPosition -> Diagram B
connectNodes s arrowOptsF n1 n2 levelled pos = connectOutside' arrowOptsF (show n1) (show n2) $ visualiseLayers s levelled

getArrowPoints :: Maybe LayerPosition -> a -> a -> a -> a
getArrowPoints posM a b c = if isJust posM 
                            then let (Just pos) = posM 
                                 in if pos == LayerLeft then a 
                                    else if pos == LayerRight then b
                                         else c 
                            else c

visualiseTree :: (Show a, Eq a, Draw a) => Settings -> [a] -> [(a,a)] -> ConnectList a -> Diagram B
visualiseTree s nodes rawConnections connectedList = outDiag <> boundingRect outDiag
    where outDiag = (if length rawConnections > 0 then connectedDiagram else visualiseLayers s levelled) # frame (fromJust . graphPadding $ s)
          connectedDiagram = foldr (\(a,b) acc -> connectOutside' arrowOpts1 (show a) (show b) acc) (visualiseLayers s levelled) rawConnections
          arrowOpts1 = with & shaftStyle %~ lw (dynamicThick s) & if directed s == Directed then headLength .~ dynamicHead s else arrowHead .~ noHead
          levelled = getLevels topList [] connectedList
          topList = nub $ getLevelList (getRoots nodes connectedList) connectedList

drawTreePartialOrder :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawTreePartialOrder = drawTreePartialOrder' defaultTreeSettings drawDefaultNode

drawTreePartialOrder' :: (Show a, Eq a, Countable a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawTreePartialOrder' settingsF drawF g = visualiseTree s nodes newConnections reduced
    where newConnections = reducedConnections reduced
          reduced = reduction (connectedTo connections) []
          (ProcessedGraph nodes connections) = getVertices drawF g
          s = settingsF g

drawTree :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawTree = drawTree' defaultTreeSettings drawDefaultNode

drawTree' :: (Show a, Eq a, Countable a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawTree' settingsF drawF g = visualiseTree s nodes connections connectedList
    where connectedList = connectedTo connections
          (ProcessedGraph nodes connections) = getVertices drawF g
          s = settingsF g

defaultTreeSettings :: (Countable a) => Graph a -> Settings
defaultTreeSettings g = Settings (dynamicStyle small $ count g) (dynamicStyle thin $ count g) Directed (Just 0.2) (Just 0.3) (Just 0.1) Nothing Nothing Nothing
