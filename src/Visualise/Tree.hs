{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module: Visualise.Tree
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Enables a graph to be drawn in a 'Tree' style. Provides four drawing
-- functions: 'drawTree' to draw the graph with default 'Settings' and 'drawTree''
-- allows the 'Settings' to be customised, as well as 'drawTreePartialOrder' 
-- which uses the Coffman-Graham algorithm to remove indirect connections
-- for directed acyclic graphs and 'drawTreePartialOrder'' which does the same
-- but allows the 'Settings' to be specified.
-- Cyclic graphs are also supported in a limited capacity.
--
-----------------------------------------------------------------------------
module Visualise.Tree (
    -- * The main drawing function: the full graph as specified with default 'Settings'.
    drawTree, 

    -- * Allows the 'Settings' to be customised.
    drawTree', 

    -- * Removes indirect connections using the Coffman-Graham algorithm before drawing.
    drawTreePartialOrder, drawTreePartialOrder', 

    -- * The default 'Settings' functions used with 'drawTree'' and 'drawTreePartialOrder''.
    defaultTreeSettings, defaultTreeSettingsHorizontal
) where

import Visualise.Common
import Algebra.Graph
import Diagrams.Prelude     hiding (Empty, union)
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
getRoots = foldr (\(x,ys) acc -> acc \\ ys)

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
-- Checks if the current vertex has any other connections other thaan the root. If it doesn't it is added to the queue. 
-- Regardless of its connections the connection between it and the root is removed from the adjacency list.
foldThroughConnectedNodes :: (Eq a) => a                     -- ^ The current root from the queue.
                                    -> [a]                   -- ^ The current queue.
                                    -> ConnectList a         -- ^ The current adjacency list.
                                    -> ([a], ConnectList a)  -- ^ The first element is the new queue and the second element is the updated adjacency list.
foldThroughConnectedNodes root queue cTo = foldl (\(accQueue,acc2) b -> -- b is the current node connected to root Node, acc1 is the current list, acc2 is the current adjacency list
                                               let edgesFrom = delete root (getEdgesFrom b acc2) in -- Gets the incoming vertices to b, but removes the root from this list
                                                   if length edgesFrom < 1 || foldr (\e boolAcc -> if loopsTo e b acc2 && boolAcc == True then True else False) True edgesFrom -- Checks if there are any other incoming vertices and if so checks if they form a loop that will end up back at b.
                                                       -- then (accQueue ++ [b], deleteConnections b (root:edgesFrom) acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
                                                      then (accQueue ++ [b], deleteConnection root b acc2) -- If the node b has no more incoming edges (or all incoming edges form loops) it is added to the list and removed from the adjacency list
                                                   else (accQueue, deleteConnection root b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
                                               (queue,cTo) (getEdgesTo root cTo)

-- | Checks if the vertex 'end' can be reached by following the outbound connections recursively from 'root'.
loopsTo :: (Eq a) => a -> a -> ConnectList a -> Bool
loopsTo root end cTo
    | end `elem` edgesFromRoot = True
    | otherwise = foldr (\n acc -> loopsTo n end cTo) False edgesFromRoot
        where edgesFromRoot = getEdgesFrom root cTo

-- | Removes a connection that goes from 'x' to 'r' in the given adjacency list 
deleteConnections :: (Eq a) => a -> [a] -> ConnectList a -> ConnectList a
deleteConnections x rs cTo = foldr (\r cToAcc -> deleteConnection x r cToAcc) cTo rs

-- | Removes a connection that goes from 'x' to 'r' in the given adjacency list 
deleteConnection :: (Eq a) => a -> a -> ConnectList a -> ConnectList a
deleteConnection x r cTo = (\(a,bs) -> if a == x then (a,delete r bs) else (a,bs)) <$> cTo

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

-- | Takes a vertex and a list of vertices (representing a layer) and returns whether the vertex is in the middle or on the left or right.
positionInGivenLayer :: (Eq a) => a -> [a] -> LayerPosition
positionInGivenLayer x ys
    | position == midPoint = LayerMiddle
    | position < midPoint = LayerLeft
    | otherwise = LayerRight
        where midPoint = fromIntegral (length ys - 1)/2
              position = fromIntegral (getElemIndex $ x `elemIndex` ys)
              getElemIndex (Just i) = i
              getElemIndex Nothing = 0

-- | Takes the visualisation settings and a list of layers and produces a diagram.
-- Folds through the level list and calls 'draw' on each vertex to get its corrisponding diagram. The diagrams in each level are horizontally separated by the amount goverened by the 'Settings' parameter.
-- The layers themselves are then vertically separated by an amount also specified by the 'Settings' parameter. 
visualiseLayers :: (Draw b) => Settings a -> [[b]] -> Diagram B
visualiseLayers s levelled = betLayerSepF (fromJust $ s ^. layerSpacing) $ foldr (\level acc -> center (inLayerSepF (fromJust $ s ^. nodeSpacing) $ draw <$> level) : acc) [] levelled
    where inLayerSepF = if fromJust (s ^. horizontalOrientation) then vsep else hsep
          betLayerSepF = if fromJust (s ^. horizontalOrientation) then hsep else vsep

-- | The main visualisation function.
-- Topologically sorts the list of vertices using 'getLevelList' before separating the vertices into layers by using 'levelled'.
-- A <https://hackage.haskell.org/package/diagrams Diagram> is then drawn without connections, using 'visualiseLayers', and then the 'rawConnections' list is folded over and the conenctions are added one by one to produce the final diagram (which is then suurrounded by a box).
visualiseTree :: (Show b, Eq b, Draw b) => Settings a      -- ^ A 'Settings' type instance providing the visualisation settings for the graph.
                                        -> [b]             -- ^ A list of the vertices in the graph -- designed to be of the type 'Node' with a String name and a 'Diagram B' diagram attribute.
                                        -> [(b,b)]         -- ^ A list of connections, with the first element being the tail and second element being the head (if the graph is 'Directed').
                                        -> ConnectList b   -- ^ The adjacency list for the graph.
                                        -> Diagram B       -- ^ The 'Diagram B' of the graph visualised as a tree.
visualiseTree s nodes rawConnections connectedListWithSelfLoops = outDiag <> boundingRect outDiag
    where outDiag = (if length rawConnections > 0 then connectedDiagram else visualiseLayers s levelled) # frame (fromJust $ s ^. graphPadding)
          -- connectedDiagram = foldr (\(a,b) acc -> connectOutside' arrowOpts1 (show a) (show b) acc) (visualiseLayers s levelled) rawConnections
          connectedDiagram = foldr (\(a,b) acc -> connectVertices s levelled a b acc) (visualiseLayers s levelled) rawConnections
          levelled = getLevels topList [] connectedList
          topList = nub $ getLevelList (getRoots nodes connectedList) connectedList
          connectedList = removeSelfLoops connectedListWithSelfLoops

-- | Removes self-loops from the given 'ConnectList' to aid graph layout.
removeSelfLoops :: (Eq a) => ConnectList a -> ConnectList a
removeSelfLoops = map (\(a,b) -> (a,delete a b))

-- | Gets the level the provided element is on in the provided list of lists.
getLevel :: (Eq a) => a -> [[a]] -> Int
getLevel x = snd . foldr (\ys (accB,accC) -> if accB == True || x `elem` ys then (True,accC) else (False,accC + 1)) (False,0)

-- | Connects two vertices with an arrow.
-- If the two vertex names are the same a self-loop is drawn, otherwise an arrow between the two nodes is drawn using the 'Settings' provided.
connectVertices :: (Show b, Eq b) => Settings a -> [[b]] -> b -> b -> Diagram B -> Diagram B
connectVertices s l a b d
    | a == b = connectPerim' arrowOpts3 (show a) (show b) (0 @@ turn) (-1/2 @@ turn) d
    | (b `getLevel` l) - (a `getLevel` l) > 1 = connectPerim' arrowOpts2 (show a) (show b) (0 @@ turn) (0 @@ turn) d
    | otherwise = connectOutside' arrowOpts1 (show a) (show b) d
        where arrowOpts1 = with & shaftStyle %~ lw (s ^. dynamicThick) & if s ^. directed == Directed then headLength .~ s ^. dynamicHead else arrowHead .~ noHead
              arrowOpts2 = with & shaftStyle %~ lw (s ^. dynamicThick) & arrowShaft .~ arc xDir (3/6 @@ turn) & if s ^. directed == Directed then headLength .~ s ^. dynamicHead else arrowHead .~ noHead
              arrowOpts3 = with & shaftStyle %~ lw (s ^. dynamicThick) & arrowShaft .~ arc xDir (4/6 @@ turn) & if s ^. directed == Directed then headLength .~ s ^. dynamicHead else arrowHead .~ noHead

-- | Removes indirect connections from the graph and produces a <https://hackage.haskell.org/package/diagrams Diagram>, using 'drawTreePartialOrder'' with the default drawing 'Settings' provided 'defaultTreeSettings'. 
-- Self-loops are not supported.
drawTreePartialOrder :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawTreePartialOrder g = drawTreePartialOrder' (defaultTreeSettings g) g

-- | Removes indirect connections from the graph and draws it using 'visualiseTree', producing a <https://hackage.haskell.org/package/diagrams Diagram>.
-- A 'Settings' instance can be provided to customise the drawing of the graph.
-- The indirect conenctions are removed by using 'reduction' and the graph is processed by 'getVertices' to produce a 'ProcessedGraph' instance containing the vertices and their connections.
-- Self-loops are not supported.
drawTreePartialOrder' :: (Show a, Eq a, Countable a) => Settings a -> Graph a -> Diagram B
drawTreePartialOrder' s g = visualiseTree s nodes newConnections reduced
    where newConnections = reducedConnections reduced
          reduced = reduction (connectedTo connections) []
          (ProcessedGraph nodes connections) = getVertices (s ^. nodeDrawFunction) g

-- | Draws the provided graph as a tree, producing a <https://hackage.haskell.org/package/diagrams Diagram> using 'drawTree'', using the default settings function 'defaultTreeSettings'. Self-loops are supported and basic cyclic functionality is also supported.
drawTree :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawTree g = drawTree' (defaultTreeSettings g) g

-- | Draws the graph provided with 'visualiseTree', using the provided visualisation 'Settings'.
-- Uses 'getVertices' to produce a 'ProcessedGraph' with the graph's verices and conenctions, then uses 'visualiseTree' with these, self-loops and basic cyclic graphs are supported.
drawTree' :: (Show a, Eq a, Countable a) => Settings a -> Graph a -> Diagram B
drawTree' s g = visualiseTree s nodes connections connectedList
    where connectedList = connectedTo connections
          (ProcessedGraph nodes connections) = getVertices (s ^. nodeDrawFunction) g


-- | Generates a default 'Settings' instance from the provided graph.
-- The arrow head size and shaft thickness vary in accordance with the graph size and the graph is 'Directed'and vertically orientated with layer separation, vertex (horizonal) separation and frame padding of 0.2, 0.3 and 0.1 respectively. 
defaultTreeSettings :: (Countable a, Show a) => Graph a -> Settings a
defaultTreeSettings g = with & dynamicHead .~ (dynamicStyle small $ count g) 
                             & dynamicThick .~ (dynamicStyle thin $ count g) 
                             & horizontalOrientation .~ Just False 
                             & layerSpacing .~ Just 0.2
                             & nodeSpacing .~ Just 0.3
                             & graphPadding .~ Just 0.1

-- | Generates a default 'Settings' instance from the provided graph, but rotated so the graph is horizonal instead of vertical.
-- The arrow head suze and shaft thickness vary in accordance with the graph size and the graph is 'Directed' and horizontally orientated with layer separation, vertex (horizonal) separation and frame padding of 0.2, 0.3 and 0.1 respectively. 
defaultTreeSettingsHorizontal :: (Countable a, Show a) => Graph a -> Settings a
defaultTreeSettingsHorizontal g = (defaultTreeSettings g) & horizontalOrientation .~ Just True