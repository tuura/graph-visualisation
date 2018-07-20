{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Visualise.DAG (
    Settings,

    drawDAG, drawDAG',-- drawDAGPartialOrder, drawDAGPartialOrder'
) where

import Visualise
import Algebra.Graph
import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG
import Diagrams.Path
import Data.List
import Data.Maybe

data Settings = Settings { layerSpacing :: Double
                         , nodeSpacing :: Double
                         , dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         }

data LayerPosition = LayerLeft | LayerMiddle | LayerRight deriving (Eq)

reduction :: ConnectList -> ConnectList -> ConnectList
reduction [] _ = []
reduction ((x,ys):zs) extras = new : reduction zs (new : extras)
    where new = (x,nub $ foldr check ys ys)
          check y accOuter = foldr (\(a,bs) acc -> if y == a then acc \\ (acc `intersect` bs) else acc) accOuter toCheck
          toCheck = extras ++ zs


getRoots :: [Node] -> ConnectList -> [Node]
getRoots = foldr (\(x,ys) acc -> delete x acc)

-- Topological sorting
getLevelList :: [Node] -> ConnectList -> [Node]
getLevelList [] _ = []
getLevelList q firstCTo = let (queue,list,cTo) = foldThroughRoots q firstCTo
                          in list ++ getLevelList queue cTo
          
foldThroughRoots :: [Node] -> ConnectList -> ([Node], [Node], ConnectList)
foldThroughRoots queue firstCTo = foldr (\root (accQueue,accList,accCTo) -> 
                                    let newList = (root : accList) -- Prepend the current root node to the list
                                        folded = foldThroughConnectedNodes root accQueue accCTo -- Fold over the nodes connected to the root node
                                    in (fst folded, newList, snd folded)) -- Add the nodes which have no other connections currently to the list
                                  ([],[],firstCTo) queue -- Start by folding over the root nodes with no incoming connections


foldThroughConnectedNodes :: Node -> [Node] -> ConnectList -> ([Node], ConnectList)
foldThroughConnectedNodes root queue cTo = foldr (\b (accQueue,acc2) -> -- b is the current node connected to root Node, acc1 is the current list, acc2 is the current adjacency list
                                              if length (getEdgesTo b acc2) <= 1 then (accQueue ++ [b], deleteConnection root b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
                                              else (accQueue, deleteConnection root b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
                                              (queue,cTo) (getEdgesFrom root cTo)

deleteConnection :: Node -> Node -> ConnectList -> ConnectList
deleteConnection x r cTo = (\(a,bs) -> (a,delete x bs)) <$> cTo

dependsOn :: Node -> Node -> ConnectList -> Bool
dependsOn a b cTo = b `elem` getEdgesTo a cTo-- If a depends on b

getEdgesTo :: Node -> ConnectList -> [Node]
getEdgesTo x cTo = if isJust $ getEdgesToTuple x cTo then let (Just tup) = getEdgesToTuple x cTo in snd tup else []

getEdgesToTuple :: Node -> ConnectList -> Maybe (Node,[Node])
getEdgesToTuple x = find (\(a,bs) -> a == x)

getEdgesFrom :: Node -> ConnectList -> [Node]
getEdgesFrom x = foldr (\(a,bs) acc -> if x `elem` bs then a : acc else acc) []

getLevels :: [Node] -> [Node] -> [(Node,[Node])] -> [[Node]]
getLevels [] _ _ = []
getLevels l lastLevel cTo = foldLevel : getLevels (l \\ foldLevel) foldLevel cTo
    where foldLevel = let (left,right,_) = foldr (\x (accL,accR,flag) -> if flag || moveToNext x accL accR then (accL,accR,True) 
                                                                         else case nodePosition x of LayerMiddle -> (accL ++ [x], accR, False)
                                                                                                     LayerRight  -> (accL, x : accR, False)
                                                                                                     LayerLeft   -> (x:accL, accR, False)
                                           ) ([],[],False) (reverse l)
                      in left ++ (reverse right)
          nodePosition x = foldr (\a acc -> if (x `dependsOn` a) cTo then a `positionInGivenLayer` lastLevel else acc) LayerMiddle lastLevel
          moveToNext x accL accR = or ((\a -> dependsOn x a cTo) <$> accL) || or ((\a -> dependsOn x a cTo) <$> accR) || any (\a -> dependsOn a x cTo) accL || any (\a -> dependsOn a x cTo) accR



reducedConnections :: ConnectList -> [(Node,Node)]
reducedConnections = foldr (\(x,ys) acc -> zip ys (repeat x) ++ acc) []

layerDiff :: Node -> Node -> [[Node]] -> Int
layerDiff a b l = fst (foldGraphLayers a l) - fst (foldGraphLayers b l)

elemPosition :: Node -> [[Node]] -> LayerPosition
elemPosition x l = snd $ foldGraphLayers x l

foldGraphLayers :: Node -> [[Node]] -> (Int,LayerPosition)
foldGraphLayers x l = let ((_,layerNum), isLeft) = fold in (layerNum, isLeft)
    where fold = foldr (\xs ((c1,c2),isLeft) -> if x `elem` xs then ((c1,c1), positionInGivenLayer x xs)
                                                else ((c1 + 1,c2),isLeft)) ((0,0),LayerMiddle) l

positionInGivenLayer :: (Node) -> [Node] -> LayerPosition
positionInGivenLayer x ys
    | position == midPoint = LayerMiddle
    | position < midPoint = LayerLeft
    | otherwise = LayerRight
        where midPoint = fromIntegral (length ys - 1)/2
              position = fromIntegral (getElemIndex $ x `elemIndex` ys)
              getElemIndex (Just i) = i
              getElemIndex Nothing = 0

visualiseLayers :: Settings -> (a -> Diagram B) -> [[Node]] -> Diagram B
-- visualiseLayers :: _
visualiseLayers s drawF levelled = vsep (layerSpacing s) $ foldl (\acc level -> center (hsep (nodeSpacing s) $ 
                                                                                       -- (draw 0.1 0.1 (drawDAG' (\_ -> s))) <$> level
                                                                                       diag <$> level
                                                                                 ) : acc)
                                                     [] levelled

-- connectNodes :: ArrowOpts Double -> String -> String -> [[String]] -> Maybe LayerPosition -> Diagram B
-- connectNodes arrowOptsF n1 n2 levelled pos = connectPerim' arrowOptsF n1 n2 degreeOne degreeTwo $ visualiseLayers levelled
--     where degreeOne = getArrowPoints pos (1/2 @@ turn) (0 @@ turn) (-1/4 @@ turn)
          -- degreeTwo = getArrowPoints pos (1/2 @@ turn) (0 @@ turn) (1/4 @@ turn)

connectNodes :: Settings -> (a -> Diagram B) -> ArrowOpts Double -> Node -> Node -> [[Node]] -> Maybe LayerPosition -> Diagram B
connectNodes s drawF arrowOptsF n1 n2 levelled pos = connectOutside' arrowOptsF (name n1) (name n2) $ visualiseLayers s drawF levelled

-- connectNodes :: Settings -> ArrowOpts Double -> String -> String -> [[String]] -> Maybe LayerPosition -> Diagram B
-- connectNodes s arrowOptsF n1 n2 levelled pos = connectOutside' arrowOptsF n1 n2 $ visualiseLayers s levelled

getArrowPoints :: Maybe LayerPosition -> a -> a -> a -> a
getArrowPoints posM a b c = if isJust posM 
                            then let (Just pos) = posM 
                                 in if pos == LayerLeft then a 
                                    else if pos == LayerRight then b
                                         else c 
                            else c

visualiseDAG :: Settings -> (a -> Diagram B) -> [Node] -> [(Node,Node)] -> ConnectList -> Diagram B
visualiseDAG s drawF nodes rawConnections connectedList = outDiag <> boundingRect outDiag
    -- where connectedDiagram = map (\(a,b) -> (if abs (layerDiff a b levelled) > 1 
    --                                          then connectNodes (arrowOpts1 & (arrowOpts2 a b)) a b levelled $ Just (elemPosition a levelled) 
    --                                          else connectNodes arrowOpts1 a b levelled Nothing)) rawConnections
    where outDiag = (if length rawConnections > 0 then mconcat connectedDiagram else visualiseLayers s drawF levelled) # frame 0.1
          connectedDiagram = map (\(a,b) -> connectNodes s drawF arrowOpts1 a b levelled Nothing) rawConnections
          arrowOpts1 = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)
          arrowOpts2 a b = let posA = elemPosition a levelled 
                               posB = elemPosition b levelled
                         in if posA == LayerLeft || (posA == LayerMiddle && posB == LayerLeft) 
                            then arrowShaft .~ arc xDir (3/12 @@ turn) 
                            else arrowShaft .~ arc xDir (-3/12 @@ turn)
          levelled = reverse . getLevels topList [] $ connectedList
          topList = reverse . nub . reverse $ getLevelList (getRoots nodes connectedList) connectedList

-- drawDAGPartialOrder :: (Show a, Eq a) => Graph a -> Diagram B
-- drawDAGPartialOrder = drawDAGPartialOrder' defaultSettings

-- drawDAGPartialOrder' :: (Show a, Eq a) => (Graph a -> Settings) -> Graph a -> Diagram B
-- drawDAGPartialOrder' settingsF g = visualiseDAG s names newConnections reduced
--     where newConnections = reducedConnections reduced
--           reduced = reduction (connectedFrom connections) []
--           names = nub namesWDuplicates
--           connections = nub connectionsWDuplicates
--           (ProcessedGraph namesWDuplicates connectionsWDuplicates) = getVertices g
--           s = settingsF g

drawDAG :: (Show a, Eq a) => (a -> Diagram B) -> Graph a -> Diagram B
drawDAG = drawDAG' defaultSettings

drawDAG' :: (Show a, Eq a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawDAG' settingsF drawF g = visualiseDAG s drawF nodes connections connectedList
    where connectedList = connectedFrom connections
          nodes = nub nodesWDuplicates
          connections = nub . reverse $ connectionsWDuplicates
          (ProcessedGraph nodesWDuplicates connectionsWDuplicates) = getVertices drawF g
          s = settingsF g

-- test s g = draw 10 10 (drawDAG' s) g


-- drawGraph :: (Eq a, Show a) => Graph a -> Diagram B 
-- drawGraph g = draw drawDAG g

defaultSettings :: Graph a -> Settings
defaultSettings g = Settings 0.2 0.3 (dynamicStyle normal $ countVertices g) (dynamicStyle thin $ countVertices g)
