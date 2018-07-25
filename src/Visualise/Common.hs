{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Visualise.Common (
    Settings(..), Node(..), ProcessedGraph(..), Directed(..),

    Dimensions, ConnectList,

    Draw,

    Countable,

    draw, count,

    drawDefaultNode, drawNodeWithEmptyFlag,

    countVertices, getVertices, connectedFrom, connectedTo, dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude hiding (Empty, union, size)
import Diagrams.Backend.SVG
import Data.List
import Data.Either
import Data.Maybe

data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         , directed :: Directed
                         , layerSpacing :: Maybe Double
                         , nodeSpacing :: Maybe Double
                         , graphPadding :: Maybe Double
                         , colF :: Maybe (Int -> Colour Double) -- For Hierarchical
                         , bgOp :: Maybe Double -- For Hierarchical
                         , initPos :: Maybe Int -- For FlatAdaptive
                         }

data Node = Node { name :: String
                 , diag :: Diagram B
                 }

data ProcessedGraph = ProcessedGraph [Node] [(Node, Node)] deriving (Show)

data Directed = Directed | Undirected deriving (Eq)

type Dimensions = (Maybe Double, Maybe Double)

type ConnectList a = [(a,[a])]

class Draw a where
    draw :: a -> Diagram B

class Countable a where
    count :: a -> Int

instance Draw Node where
    draw (Node _ d) = d

instance Eq Node where
    a == b = name a == name b

instance Show Node where
    show = name

instance Ord Node where
    (Node n1 _) `compare` (Node n2 _) = n1 `compare` n2

instance (Countable a) => Countable (Graph a) where
    count g = foldg 1 count (+) (+) g

instance Countable String where
    count _ = 1

instance Countable Int where
    count _ = 1

instance Countable Char where
    count _ = 1

instance Countable (String,Bool) where
    count _ = 1

countVertices :: Graph a -> Int
countVertices = size

drawDefaultNode :: (Show a) => a -> Diagram B
drawDefaultNode nn = (text n # fontSizeL 0.1 <> circle 0.1) # href ("javascript:alert(\"Node " ++ n ++ "\")")
    where n = show nn

drawNodeWithEmptyFlag :: (Show a) => (a,Bool) -> Diagram B
drawNodeWithEmptyFlag (n,f) = let txt = if f then "" else show n 
                              in (text txt # fontSizeL 0.1 <> circle 0.1) # href ("javascript:alert(\"Node " ++ (show n) ++ "\")")

getNode :: (Show a) => (a -> Diagram B) -> Either String a -> Node
getNode drawF x
    | isRight x = Node (show . head . rights $ [x]) ((drawF . head . rights $ [x]) # named (show . head . rights $ [x]))
    | otherwise = Node (fromLeft "" x) ((drawDefaultNode "") # named (fromLeft "" x))

getVertices :: (Eq a, Show a) => (a -> Diagram B) -> Graph a -> ProcessedGraph
getVertices drawF g = namesAndConnections drawF g ""

namesAndConnections :: (Eq a, Show a) => (a -> Diagram B) -> Graph a -> String -> ProcessedGraph
namesAndConnections drawF Empty c = ProcessedGraph [getNode drawF (Left ("_empty_node_" ++ c))] []
namesAndConnections drawF v@(Vertex a) c = ProcessedGraph [getNode drawF (Right a)] []
namesAndConnections drawF (Overlay a b) c = ProcessedGraph (nA `union` nB) (cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections drawF a ('l' : c)
          (ProcessedGraph nB cB) = namesAndConnections drawF b ('r' : c)
namesAndConnections drawF (Connect a b) c = ProcessedGraph (nA `union` nB) ([(aA, bB) | aA <- nA, bB <- nB] `union` cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections drawF a ('l' : c)
          (ProcessedGraph nB cB) = namesAndConnections drawF b ('r' : c)

connectedFrom :: (Eq a) => [(a,a)] -> ConnectList a
connectedFrom [] = []
connectedFrom [(a,b),(x,y)]
  | b == y = [(b,[a,x])]
  | otherwise = (b,[a]) : [(y,[x])]
connectedFrom l@((x,y):zs) = (y,incoming) : if not (null remaining) then connectedFrom remaining else []
    where remaining = zs \\ filtered
          incoming = foldr (\(a,b) acc -> a : acc) [] filtered
          filtered = filter (\(a,b) -> b == y) l

connectedTo :: (Eq a) => [(a,a)] -> ConnectList a
connectedTo [] = []
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if not (null remaining) then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l

dynamicStyle :: Measure Double -> Int -> Measure Double
dynamicStyle def graphSize = def * 10/fromIntegral graphSize