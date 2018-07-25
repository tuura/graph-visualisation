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

data LayerPosition = LayerLeft | LayerMiddle | LayerRight deriving (Eq)

reduction :: (Eq a) => ConnectList a -> ConnectList a -> ConnectList a
reduction [] _ = []
reduction ((x,ys):zs) extras = new : reduction zs (new : extras)
    where new = (x,nub $ foldr check ys ys)
          check y accOuter = foldr (\(a,bs) acc -> if y == a then acc \\ (acc `intersect` bs) else acc) accOuter toCheck
          toCheck = extras ++ zs


getRoots :: (Eq a) => [a] -> ConnectList a -> [a]
getRoots = foldr (\(x,ys) acc -> delete x acc)

-- Topological sorting
getLevelList :: (Eq a) => [a] -> ConnectList a -> [a]
getLevelList [] _ = []
getLevelList q firstCTo = let (queue,list,cTo) = foldThroughRoots q firstCTo
                          in list ++ getLevelList queue cTo
          
foldThroughRoots :: (Eq a) => [a] -> ConnectList a -> ([a], [a], ConnectList a)
foldThroughRoots queue firstCTo = foldr (\root (accQueue,accList,accCTo) -> 
                                    let newList = (root : accList) -- Prepend the current root node to the list
                                        folded = foldThroughConnectedNodes root accQueue accCTo -- Fold over the nodes connected to the root node
                                    in (fst folded, newList, snd folded)) -- Add the nodes which have no other connections currently to the list
                                  ([],[],firstCTo) queue -- Start by folding over the root nodes with no incoming connections


foldThroughConnectedNodes :: (Eq a) => a -> [a] -> ConnectList a -> ([a], ConnectList a)
foldThroughConnectedNodes root queue cTo = foldr (\b (accQueue,acc2) -> -- b is the current node connected to root Node, acc1 is the current list, acc2 is the current adjacency list
                                              if length (getEdgesTo b acc2) <= 1 then (accQueue ++ [b], deleteConnection root b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
                                              else (accQueue, deleteConnection root b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
                                              (queue,cTo) (getEdgesFrom root cTo)

deleteConnection :: (Eq a) => a -> a -> ConnectList a -> ConnectList a
deleteConnection x r cTo = (\(a,bs) -> (a,delete x bs)) <$> cTo

dependsOn :: (Eq a) => a -> a -> ConnectList a -> Bool
dependsOn a b cTo = b `elem` getEdgesTo a cTo-- If a depends on b

getEdgesTo :: (Eq a) => a -> ConnectList a -> [a]
getEdgesTo x cTo = if isJust $ getEdgesToTuple x cTo then let (Just tup) = getEdgesToTuple x cTo in snd tup else []

getEdgesToTuple :: (Eq a) => a -> ConnectList a -> Maybe (a,[a])
getEdgesToTuple x = find (\(a,bs) -> a == x)

getEdgesFrom :: (Eq a) => a -> ConnectList a -> [a]
getEdgesFrom x = foldr (\(a,bs) acc -> if x `elem` bs then a : acc else acc) []

getLevels :: (Eq a) => [a] -> [a] -> [(a,[a])] -> [[a]]
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

reducedConnections :: ConnectList a -> [(a,a)]
reducedConnections = reverse . foldr (\(x,ys) acc -> zip ys (repeat x) ++ acc) []

layerDiff :: (Eq a) => a -> a -> [[a]] -> Int
layerDiff a b l = fst (foldGraphLayers a l) - fst (foldGraphLayers b l)

elemPosition :: (Eq a) => a -> [[a]] -> LayerPosition
elemPosition x l = snd $ foldGraphLayers x l

foldGraphLayers :: (Eq a) => a -> [[a]] -> (Int,LayerPosition)
foldGraphLayers x l = let ((_,layerNum), isLeft) = fold in (layerNum, isLeft)
    where fold = foldr (\xs ((c1,c2),isLeft) -> if x `elem` xs then ((c1,c1), positionInGivenLayer x xs)
                                                else ((c1 + 1,c2),isLeft)) ((0,0),LayerMiddle) l

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

-- connectNodes :: ArrowOpts Double -> String -> String -> [[String]] -> Maybe LayerPosition -> Diagram B
-- connectNodes arrowOptsF n1 n2 levelled pos = connectPerim' arrowOptsF n1 n2 degreeOne degreeTwo $ visualiseLayers levelled
--     where degreeOne = getArrowPoints pos (1/2 @@ turn) (0 @@ turn) (-1/4 @@ turn)
          -- degreeTwo = getArrowPoints pos (1/2 @@ turn) (0 @@ turn) (1/4 @@ turn)

connectNodes :: (Show a, Draw a) => Settings -> ArrowOpts Double -> a -> a -> [[a]] -> Maybe LayerPosition -> Diagram B
connectNodes s arrowOptsF n1 n2 levelled pos = connectOutside' arrowOptsF (show n1) (show n2) $ visualiseLayers s levelled

-- connectNodes :: Settings -> ArrowOpts Double -> String -> String -> [[String]] -> Maybe LayerPosition -> Diagram B
-- connectNodes s arrowOptsF n1 n2 levelled pos = connectOutside' arrowOptsF n1 n2 $ visualiseLayers s levelled

getArrowPoints :: Maybe LayerPosition -> a -> a -> a -> a
getArrowPoints posM a b c = if isJust posM 
                            then let (Just pos) = posM 
                                 in if pos == LayerLeft then a 
                                    else if pos == LayerRight then b
                                         else c 
                            else c

visualiseTree :: (Show a, Eq a, Draw a) => Settings -> [a] -> [(a,a)] -> ConnectList a -> Diagram B
visualiseTree s nodes rawConnections connectedList = outDiag <> boundingRect outDiag
    -- where connectedDiagram = map (\(a,b) -> (if abs (layerDiff a b levelled) > 1 
    --                                          then connectNodes (arrowOpts1 & (arrowOpts2 a b)) a b levelled $ Just (elemPosition a levelled) 
    --                                          else connectNodes arrowOpts1 a b levelled Nothing)) rawConnections
    where outDiag = (if length rawConnections > 0 then mconcat connectedDiagram else visualiseLayers s levelled) # frame (fromJust . graphPadding $ s)
          connectedDiagram = map (\(a,b) -> connectNodes s arrowOpts1 a b levelled Nothing) rawConnections
          arrowOpts1 = with & shaftStyle %~ lw (dynamicThick s) & if directed s == Directed then headLength .~ dynamicHead s else arrowHead .~ noHead
          arrowOpts2 a b = let posA = elemPosition a levelled 
                               posB = elemPosition b levelled
                         in if posA == LayerLeft || (posA == LayerMiddle && posB == LayerLeft) 
                            then arrowShaft .~ arc xDir (3/12 @@ turn) 
                            else arrowShaft .~ arc xDir (-3/12 @@ turn)
          levelled = reverse . getLevels topList [] $ connectedList
          topList = reverse . nub . reverse $ getLevelList (getRoots nodes connectedList) connectedList

drawTreePartialOrder :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawTreePartialOrder = drawTreePartialOrder' defaultTreeSettings drawDefaultNode

drawTreePartialOrder' :: (Show a, Eq a, Countable a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawTreePartialOrder' settingsF drawF g = visualiseTree s nodes newConnections reduced
    where newConnections = reducedConnections reduced
          reduced = reduction (connectedFrom connections) []
          (ProcessedGraph nodes connections) = getVertices drawF g
          s = settingsF g

drawTree :: (Show a, Eq a, Countable a) => Graph a -> Diagram B
drawTree = drawTree' defaultTreeSettings drawDefaultNode

drawTree' :: (Show a, Eq a, Countable a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawTree' settingsF drawF g = visualiseTree s nodes (reverse connections) connectedList
    where connectedList = connectedFrom (reverse connections)
          (ProcessedGraph nodes connections) = getVertices drawF g
          s = settingsF g

defaultTreeSettings :: (Countable a) => Graph a -> Settings
defaultTreeSettings g = Settings (dynamicStyle small $ count g) (dynamicStyle thin $ count g) Directed (Just 0.2) (Just 0.3) (Just 0.1) Nothing Nothing Nothing
