{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Visualise (
    ProcessedGraph(..),

    Node, Dimensions, ConnectList,
    name, diag,

    -- Named,

    -- VertexContents,
    -- Draw,
    draw, 
    drawNode,-- drawGraph,
    drawDefaultNode,

    getNode,

    saveSVG, countVertices, getVertices, connectedFrom, connectedTo, dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude hiding (Empty, union)
import Diagrams.Backend.SVG
import Data.List

data Node = Node { name :: String
                 , diag :: Diagram B
                 } 

instance Eq Node where
    a == b = name a == name b

instance Show Node where
    show = name

data ProcessedGraph = ProcessedGraph [Node] [(Node, Node)] deriving (Show)


type Dimensions = (Maybe Double, Maybe Double)

type ConnectList = [(Node,[Node])]

-- class VertexContents a where
--     getName :: a -> String
--     getConnections :: a -> [(String,String)]
--     draw :: Double -> Double -> (Graph b -> Diagram B) -> a -> Diagram B

-- instance VertexContents String where
--     getName x = x
--     getConnections _ = []
--     draw fS cS _ n = (text (getName n) # fontSizeL fS <> circle cS) # named (getName n) # href ("javascript:alert(\"Node " ++ (getName n) ++ "\")")

-- instance VertexContents Char where
--     getName x = x : []
--     getConnections _ = []
--     draw fS cS _ n = (text (getName n) # fontSizeL fS <> circle cS) # named (getName n) # href ("javascript:alert(\"Node " ++ (getName n) ++ "\")")

-- instance VertexContents Int where
--     getName x = show x
--     getConnections _ = []
--     draw fS cS _ n = (text (getName n) # fontSizeL fS <> circle cS) # named (getName n) # href ("javascript:alert(\"Node " ++ (getName n) ++ "\")")

-- instance (Eq a, VertexContents a) => VertexContents (Graph a) where
--     getName g = unwords names
--         where (ProcessedGraph names _) = getVertices g

--     getConnections g = []
--         where (ProcessedGraph names connections) = getVertices g

--     draw :: Double -> Double -> (Graph a -> Diagram B) -> Graph a -> Diagram B
--     draw fS cS diagF g = (diagF g <> circle cS)-- # named n # href ("javascript:alert(\"Node " ++ n ++ "\")")




saveSVG :: FilePath -> Dimensions -> Diagram B -> IO ()
saveSVG path (w,h) d = renderSVG path (mkSizeSpec2D w h) d

countVertices :: Graph a -> Int
countVertices Empty = 1
countVertices (Vertex a) = 1
countVertices (Overlay a b) = countVertices a + countVertices b
countVertices (Connect a b) = countVertices a + countVertices b



-- class Draw a where
--     draw :: (a -> (b -> Diagram B) -> Diagram B) -> a -> Diagram B
--     getName :: a -> String

-- instance (Show a, Eq a, Draw a, Ord a) => Draw (Graph (Graph a)) where
--     draw drawF g = drawF draw g
--     getName g = unwords names
--         where (ProcessedGraph names _) = getVertices g

-- instance (Show a, Eq a, Draw a) => Draw (Graph a) where
--     draw drawF g = drawF drawNode $ show <$> g

-- instance Draw String where
-- drawNode :: (Show a) => a -> Diagram B
-- drawNode nn = (text n # fontSizeL 0.1 <> circle 0.1) # named n # href ("javascript:alert(\"Node " ++ n ++ "\")")
--     where n = show nn

class Draw a where
    drawNode :: (a -> Diagram B) -> a -> Diagram B

instance Draw (Graph a) where
    drawNode drawN g = drawN g

instance Draw String where
    drawNode _ g = drawDefaultNode g

drawDefaultNode :: String -> Diagram B
drawDefaultNode n = (text n # fontSizeL 0.1 <> circle 0.1) # href ("javascript:alert(\"Node " ++ n ++ "\")")
-- drawDefaultNode n = (text n # fontSizeL 0.1 <> circle 0.1) # named (show n) # href ("javascript:alert(\"Node " ++ n ++ "\")")


-- drawVertexGraph :: ((b -> Diagram B) -> Graph a -> Diagram B) -> (b -> Diagram B) -> Graph (Graph b) -> Diagram B
-- drawVertexGraph drawF nodeF g = drawF (drawGraph drawF nodeF) g

-- drawVertexGraph :: ((Graph b -> Diagram B) -> Graph b -> Diagram B) -> (a -> Diagram B) -> Graph (Graph a) -> Diagram B
-- drawVertexGraph drawF nodeF g = drawF (drawGraph drawF nodeF) g

-- drawGraph :: ((a -> Diagram B) -> Graph a -> Diagram B) -> (a -> Diagram B) -> Graph a -> Diagram B
-- drawGraph drawF nodeF g = drawF nodeF g

draw :: (Eq a, Show a) => ((a -> Diagram B) -> Graph a -> Diagram B) -> (a -> Diagram B) -> Graph a -> Diagram B
draw drawF drawN g = diag $ getNode (drawF drawN) g







-- class Named a where
--     getNode :: (a -> Diagram B) -> a -> Node

-- -- instance (Eq a, Show a) => Named (Graph (Graph a)) where
-- --     getNode drawF drawN g = Node (unwords $ name <$> gNodes) (drawF (drawF drawN) g)
-- --         where (ProcessedGraph gNodes _) = getVertices drawF g

-- instance (Show a) => Named (Graph a) where
--     getNode drawF g = Node (show g) (drawF g)

-- instance Named String where
--     -- getNode drawF drawN v@(Vertex (Vertex n)) = Node (show n) (drawF v)
--     -- getNode v@(Vertex n) drawF drawN = Node (show n) (drawN n)
--     getNode drawF n = Node n (drawF n)

-- instance Named Int where
--     -- getNode drawF drawN v@(Vertex (Vertex n)) = Node (show n) (drawF v)
--     -- getNode v@(Vertex n) drawF drawN = Node (show n) (drawN n)
--     getNode drawF n = Node (show n) (drawF n)




getNode :: (Show a) => (a -> Diagram B) -> a -> Node
getNode drawF x = Node (show x) ((drawF x) # named (show x))

getVertices :: (Eq a, Show a) => (a -> Diagram B) -> Graph a -> ProcessedGraph
getVertices = namesAndConnections

namesAndConnections :: (Eq a, Show a) => (a -> Diagram B) -> Graph a -> ProcessedGraph
-- namesAndConnections Empty c = ProcessedGraph ["_empty_node_" ++ c] []
namesAndConnections drawF v@(Vertex a) = ProcessedGraph [getNode drawF a] []
namesAndConnections drawF (Overlay a b) = ProcessedGraph (nA `union` nB) (cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections drawF a
          (ProcessedGraph nB cB) = namesAndConnections drawF b
namesAndConnections drawF (Connect a b) = ProcessedGraph (nA `union` nB) ([(aA, bB) | aA <- nA, bB <- nB] `union` cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections drawF a
          (ProcessedGraph nB cB) = namesAndConnections drawF b

-- namesAndConnections :: (Show a) => Graph a -> String -> ProcessedGraph String
-- namesAndConnections Empty c = ProcessedGraph ["_empty_node_" ++ c] []
-- namesAndConnections (Vertex a) c = ProcessedGraph [show a] []
-- namesAndConnections (Overlay a b) c = ProcessedGraph (nA `union` nB) (cA `union` cB)
--     where (ProcessedGraph nA cA) = namesAndConnections a (c ++ "_l")
--           (ProcessedGraph nB cB) = namesAndConnections b (c ++ "_r")
-- namesAndConnections (Connect a b) c = ProcessedGraph (nA `union` nB) ([(aA, bB) | aA <- nA, bB <- nB] `union` cA `union` cB)
--     where (ProcessedGraph nA cA) = namesAndConnections a (c ++ "_l")
--           (ProcessedGraph nB cB) = namesAndConnections b (c ++ "_r")

connectedFrom :: [(Node,Node)] -> ConnectList
connectedFrom [] = []
connectedFrom [(a,b),(x,y)]
  | b == y = [(b,[a,x])]
  | otherwise = (b,[a]) : [(y,[x])]
connectedFrom l@((x,y):zs) = (y,incoming) : if not (null remaining) then connectedFrom remaining else []
    where remaining = zs \\ filtered
          incoming = foldr (\(a,b) acc -> a : acc) [] filtered
          filtered = filter (\(a,b) -> b == y) l

connectedTo :: [(Node,Node)] -> ConnectList
connectedTo [] = []
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if not (null remaining) then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l

-- node :: Double -> Double -> String -> Diagram B
-- node fS cS n = (text nodeText # fontSizeL fS <> circle cS) # named n # href ("javascript:alert(\"Node " ++ n ++ "\")")
--     where nodeText = if "_empty_node_" `isPrefixOf` n then "" else n

dynamicStyle :: Measure Double -> Int -> Measure Double
dynamicStyle def graphSize = def * 10/fromIntegral graphSize

