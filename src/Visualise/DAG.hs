{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Visualise.DAG (
    Settings,

    drawDAG, drawDAG'--, drawDAGPartialOrder, drawDAGPartialOrder'
) where

import Visualise
import Algebra.Graph
import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG
import Diagrams.Path
import Data.List
import Data.Maybe

data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                     }

data LayerPosition = LayerLeft | LayerMiddle | LayerRight deriving (Eq)

reduction :: (Show a, Eq a) => ConnectList a -> ConnectList a -> ConnectList a
reduction [] _ = []
reduction ((x,ys):zs) extras = new : reduction zs (new : extras)
    where new = (x,nub $ foldr check ys ys)
          check y accOuter = foldr (\(a,bs) acc -> if y == a then acc \\ (acc `intersect` bs) else acc) accOuter toCheck
          toCheck = extras ++ zs


getRoots :: (Show a, Eq a) => [a] -> ConnectList a -> [a]
getRoots = foldr (\(x,ys) acc -> delete x acc)

-- Topological sorting
getLevelList :: (Show a, Eq a) => [a] -> ConnectList a -> [a]
getLevelList [] _ = []
getLevelList q firstCTo = let (queue,list,cTo) = foldThroughRoots q firstCTo
                          in list ++ getLevelList queue cTo
          
foldThroughRoots :: (Show a, Eq a) => [a] -> ConnectList a -> ([a], [a], ConnectList a)
foldThroughRoots queue firstCTo = foldr (\root (accQueue,accList,accCTo) -> 
                                    let newList = (accList ++ [root]) -- Prepend the current root node to the list
                                        folded = foldThroughConnectedNodes root accQueue accCTo -- Fold over the nodes connected to the root node
                                    in (fst folded, newList, snd folded)) -- Add the nodes which have no other connections currently to the list
                                    ([],[],firstCTo) queue -- Start by folding over the root nodes with no incoming connections


foldThroughConnectedNodes :: (Show a, Eq a) => a -> [a] -> ConnectList a -> ([a], ConnectList a)
foldThroughConnectedNodes root queue cTo = foldr (\b (accQueue,acc2) -> -- b is the current node connected to root node a, acc1 is the current list, acc2 is the current adjacency list
                                              if length (getEdgesTo b acc2) <= 1 then (accQueue ++ [b], deleteConnection root b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
                                              else (accQueue, deleteConnection root b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
                                              (queue,cTo) (getEdgesFrom root cTo)

deleteConnection :: (Show a, Eq a) => a -> a -> ConnectList a -> ConnectList a
deleteConnection x r cTo = (\(a,bs) -> (a,delete x bs)) <$> cTo

dependsOn :: (Show a, Eq a) => a -> a -> ConnectList a -> Bool
dependsOn a b cTo = b `elem` getEdgesTo a cTo-- If a depends on b

getEdgesTo :: (Show a, Eq a) => a -> ConnectList a -> [a]
getEdgesTo x cTo = if isJust $ getEdgesToTuple x cTo then let (Just tup) = getEdgesToTuple x cTo in snd tup else []

getEdgesToTuple :: (Show a, Eq a) => a -> ConnectList a -> Maybe (a,[a])
getEdgesToTuple x = find (\(a,bs) -> a == x)

getEdgesFrom :: (Show a, Eq a) => a -> ConnectList a -> [a]
getEdgesFrom x = foldr (\(a,bs) acc -> if x `elem` bs then a : acc else acc) []

getLevels :: (Show a, Eq a) => [a] -> [a] -> [(a,[a])] -> [[a]]
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



reducedConnections :: (Show a, Eq a) => ConnectList a -> [(a,a)]
reducedConnections = foldr (\(x,ys) acc -> zip ys (repeat x) ++ acc) []

layerDiff :: (Show a, Eq a) => a -> a -> [[a]] -> Int
layerDiff a b l = fst (foldGraphLayers a l) - fst (foldGraphLayers b l)

elemPosition :: (Show a, Eq a) => a -> [[a]] -> LayerPosition
elemPosition x l = snd $ foldGraphLayers x l

foldGraphLayers :: (Show a, Eq a) => a -> [[a]] -> (Int,LayerPosition)
foldGraphLayers x l = let ((_,layerNum), isLeft) = fold in (layerNum, isLeft)
    where fold = foldr (\xs ((c1,c2),isLeft) -> if x `elem` xs then ((c1,c1), positionInGivenLayer x xs)
                                                else ((c1 + 1,c2),isLeft)) ((0,0),LayerMiddle) l

positionInGivenLayer :: (Eq a) => a -> [a] -> LayerPosition
positionInGivenLayer x ys
    | position == midPoint = LayerMiddle
    | position < midPoint = LayerLeft
    | otherwise = LayerRight
        where midPoint = fromIntegral (length ys - 1)/2
              position = fromIntegral (getElemIndex $ x `elemIndex` ys)
              getElemIndex (Just i) = i
              getElemIndex Nothing = 0

visualiseLayers :: [[String]] -> Diagram SVG
visualiseLayers levelled = vsep 0.2 $ foldl (\acc level -> center (hsep 0.3 $ node 0.1 0.1 <$> level) : acc) [] levelled

connectNodes :: ArrowOpts Double -> String -> String -> [[String]] -> Maybe LayerPosition -> Diagram B
connectNodes arrowOptsF n1 n2 levelled pos = connectPerim' arrowOptsF n1 n2 degreeOne degreeTwo $ visualiseLayers levelled
    where degreeOne = getArrowPoints pos (1/2 @@ turn) (0 @@ turn) (-1/4 @@ turn)
          degreeTwo = getArrowPoints pos (1/2 @@ turn) (0 @@ turn) (1/4 @@ turn)

getArrowPoints :: Maybe LayerPosition -> a -> a -> a -> a
getArrowPoints posM a b c = if isJust posM 
                            then let (Just pos) = posM 
                                 in if pos == LayerLeft then a 
                                    else if pos == LayerRight then b
                                         else c 
                            else c

visualiseDAG :: Settings -> [String] -> [(String,String)] -> ConnectList String -> Diagram B
visualiseDAG s names rawConnections connectedList = mconcat connectedDiagram # frame 0.1
    where connectedDiagram = map (\(a,b) -> (if abs (layerDiff a b levelled) > 1 
                                             then connectNodes (arrowOpts1 & (arrowOpts2 a b)) a b levelled $ Just (elemPosition a levelled) 
                                             else connectNodes arrowOpts1 a b levelled Nothing)) rawConnections
          arrowOpts1 = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)
          arrowOpts2 a b = let posA = elemPosition a levelled 
                               posB = elemPosition b levelled
                         in if posA == LayerLeft || (posA == LayerMiddle && posB == LayerLeft) 
                            then arrowShaft .~ arc xDir (3/12 @@ turn) 
                            else arrowShaft .~ arc xDir (-3/12 @@ turn)
          levelled = reverse . getLevels topList [] $ connectedList
          topList = reverse . nub . reverse $ getLevelList (getRoots names connectedList) connectedList

drawDAGPartialOrder :: (Show a) => Graph a -> Diagram B
drawDAGPartialOrder g = drawDAGPartialOrder' (defaultSettings g) g

drawDAGPartialOrder' :: (Show a) => Settings -> Graph a -> Diagram B
drawDAGPartialOrder' s graph = visualiseDAG s names newConnections reduced
    where newConnections = reducedConnections reduced
          reduced = reduction (connectedFrom connections) []
          names = nub namesWDuplicates
          connections = nub connectionsWDuplicates
          (ProcessedGraph namesWDuplicates connectionsWDuplicates) = getVertices g
          g = show <$> graph

drawDAG :: (Show a) => Graph a -> Diagram B
drawDAG g = drawDAG' (defaultSettings g) g

drawDAG' :: (Show a) => Settings -> Graph a -> Diagram B
drawDAG' s graph = visualiseDAG s names connections connectedList
    where connectedList = connectedFrom connections
          names = nub namesWDuplicates
          connections = nub . reverse $ connectionsWDuplicates
          (ProcessedGraph namesWDuplicates connectionsWDuplicates) = getVertices g
          g = show <$> graph

defaultSettings :: Graph a -> Settings
defaultSettings g = Settings (dynamicStyle normal $ countVertices g) 
                           (dynamicStyle thin $ countVertices g)
