{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module: Visualise.Common
-- Copyright : (c) Samuel Prescott 2018
-- 
-- Provides common types, typeclasses and functions used by the various drawing
-- modules.
--
-----------------------------------------------------------------------------
module Visualise.Common (
    -- * The 'Settings' type used by most drawing modules to customise the drawing of a graph.
    Settings(..), 

    -- * The 'Node' type which is used by the drawing modules to represent a vertex, containing a 'String' representation and a <https://hackage.haskell.org/package/diagrams Diagram> representation.
    Node(..), 

    -- * The 'ProcessedGraph' type which is used by 'getVertices' to represent a graph that has been folded through, with a list of vertices and a list of conenctions.
    ProcessedGraph(..), 

    -- * The 'Boolean' type 'Directed' which determines whether a graph is directed/has arrows on connections.
    Directed(..),

    -- * An array of tuples representing connections in a graph.
    ConnectList,

    -- * The 'Draw' TypeClass which determines how to draw a 'Node' using the function 'draw'.
    Draw, draw,

    -- * The 'Countable' TypeClass which determines how to count the number of vertices in a graph/graph of graphs using the function 'count'.
    Countable, count,

    -- * The default vertex-drawing functions.
    drawDefaultNode, drawDefaultNodeWithSize, drawDefaultEmptyNode, drawNodeWithEmptyFlag,

    -- * Gets a 'ProcessedGraph' containing a graph's vertices and connections, a wrapper for 'namesAndConnections'.
    getVertices, 

    -- * Gets a 'ProcessedGraph' containing a graph's vertices and connections, recursive requiring a counter string.
    namesAndConnections, 

    -- * Gets the vertices connected from and to a specified vertex.
    connectedFrom, connectedTo, 

    -- * Used to adapt graph-drawing parameters in accordance with the size of a graph.
    dynamicStyle
) where

import Algebra.Graph
import Diagrams.Prelude     hiding (Empty, union)
import Diagrams.Backend.SVG
import Data.List
import Data.Either
import Data.Maybe

-- | The 'Settings' data type contains various fields for the graph drawing methods, some common between them and some specialised to a single method.
-- To give custom settings to the 'drawGraph'' function, a function which takes a graph as an argument and returns a 'Settings' instance will be given to it.
data Settings = 
  Settings { -- | The size of the arrow heads for the graph drawing, useful to scale with graph size. 
             -- The default implementation for each of the drawing methods is similar, being based on:
             -- @
             -- dynamicStyle small $ count g
             -- @
             -- With 'normal' being used in place of 'small' for some methods.
             dynamicHead :: Measure Double
             -- | The thickness of the arrow shafts for the graph drawing, again useful to scale like 'dynamicHead' like so:
             -- @
             -- dynamicStyle thin $ count g
             -- @
           , dynamicThick :: Measure Double
             -- | Whether the graph is 'Directed' (arrows on the connections between nodes). The type 'Directed' is a boolean value that can either be 'Directed' or 'Undirected'.
           , directed :: Directed
             -- | The orientation of the graph when using the "Visualise.Tree" module's functions.
           , horizontalOrientation :: Maybe Bool
             -- | The spacing between layers when using the "Visualise.Tree" module's functions.
           , layerSpacing :: Maybe Double
             -- | The spacing between vertices when using the "Visualise.Tree" module's functions.
           , nodeSpacing :: Maybe Double
             -- | The padding around the graph when using the "Visualise.Tree" module's functions.
           , graphPadding :: Maybe Double
             -- | The background colour of groups when using the "Visualise.Hierarchical" module.
           , colF :: Maybe (Int -> Colour Double)
             -- | The background opacity of groups when using the "Visualise.Hierarchical" module.
           , bgOp :: Maybe Double
             -- | The mode of initial positioning for the vertices when using "Visualise.FlatAdaptive", changing this results in differently layed out graphs. 
             -- Useful to experiment with to best display a graph.
           , initPos :: Maybe Int
           }

-- | The 'Node' data type represents a node/vertex on a graph, storing the node's String name and its corrisponding diagram from the "Diagrams" library.
data Node = 
  Node { -- | Use 'name' to get the String representation of a node.
         name :: String
         -- | Use 'diag' to get the "Diagrams" representation of a node.
       , diag :: Diagram B
       }

-- | The 'ProcessedGraph' data type contains a list of a graph's nodes along with the connections between them in a list of tuples.
data ProcessedGraph = ProcessedGraph [Node] [(Node, Node)] deriving (Show)

-- | The 'Directed' data type is a boolean type which is used to determine if a graph is a directed or undiercted graph.
data Directed = Directed | Undirected deriving (Eq)

-- | The type 'ConnectList' is an alias for an array of tuples that can be used where the first element in each tuple is a vertex and the second element is an array of the connected vertices - i.e. an adjacency list if the connected bvertices are only vertices dependant on the vertex.
type ConnectList a = [(a,[a])]

-- | The TypeClass 'Draw' is used to determine whether a value can be drawn, this is used for vertices as these must be able to be drawn.
class Draw a where
    -- | The draw function produces a <https://hackage.haskell.org/package/diagrams Diagram> from the drawable value.
    draw :: a -> Diagram B

-- | The type 'Node' is an instance of the 'Draw' class as it can be drawn.
instance Draw Node where
    -- | The diagram is extracted from the 'Node'
    draw (Node _ d) = d

-- | The TypeClass 'Countable' is used to determine if a type can be counted when counting the number of vertices in a graph/more specificaly a nested graph.
class Countable a where
    -- | The count function gives an Integer representing the number of vertices in a.
    count :: a -> Int

-- | The type 'Graph a' where 'a' itself is an instance of 'Countable' is an instance of the TypeClass 'Countable'.
instance (Countable a) => Countable (Graph a) where
    -- | To count the number of vertices in the given graph, the graph is folded over using "Algebra.Graph"'s 'foldg' function.
    -- Empty vertices are counted as 1, each vertex has 'count' called recursively called on it (for if the graph is a graph of graphs) and for overlays and connections the number of vertices in each branch are added.
    count = foldg 1 count (+) (+)

-- | The possible types for vertex contents are also instances of 'Countable', starting with 'String'.
instance Countable String where
    -- | A String can only be a single Vertex so is counted as 1.
    count _ = 1

instance Countable Int where
    count _ = 1

instance Countable Char where
    count _ = 1

-- | This type is a tuple of a String (the contents of the vertex) and a boolean flag that can be used to choose whether to display the Vertex with the String name or as an Empty Vertex.
instance Countable (String,Bool) where
    -- | Can only be a single Vertex, so counted as 1.
    count _ = 1

-- | 'Node's can naturally be equated so are instances of the Eq TypeClass.
instance Eq Node where
    -- | Compares the String element of the two 'Node's, ignoring the Diagram element.
    a == b = name a == name b

-- | 'Node's need to be shown in order to obtain a String representation of them for use as a SubDiagram name to enable the linking of vertices with arrows
instance Show Node where
    -- | Simply gives the 'name' element of the 'Node'
    show = name

-- | 'Node's need to be an instance of the Ord typeclass for use with the "Visualise.GraphViz" module
instance Ord Node where
    -- | To compare the ordering of two nodes, their Diagram elements are ignored and their String 'name's are compared.
    a `compare` b = show a `compare` show b

-- | Produces a default node á la 'drawDefaultNodeWithSize' with a size of '0.1'.
drawDefaultNode :: (Show a) => a -> Diagram B
drawDefaultNode n = drawDefaultNodeWithSize 0.1 n

-- | The default drawing function to produce a single node but of the specified size, in the form of a 'Diagram B', containing some showable text.
-- Also makes the node a link, so if the drawn graph is saved as an SVG file each vertex is clickable with a JavaScript alert giving the vertex's name.
drawDefaultNodeWithSize :: (Show a) => Double -> a -> Diagram B
drawDefaultNodeWithSize s nn = (text n # fontSizeL s <> drawDefaultEmptyNode s) # href ("javascript:alert(\"Node " ++ n ++ "\")")
    where n = show nn

-- | The default circle <https://hackage.haskell.org/package/diagrams Diagram> for an empty node of the size provided.
drawDefaultEmptyNode :: Double -> Diagram B
drawDefaultEmptyNode = circle

-- | The same as 'drawDefaultNode' but instead takes a tuple of a showable value and a boolean flag. 
-- If the flag is True then it means the vertex should be displayed as an empty vertex with no contents, but if the flag is true the vertex is drawn as normal.
drawNodeWithEmptyFlag :: (Show a) => (a,Bool) -> Diagram B
drawNodeWithEmptyFlag (n,f) = let txt = if f then "" else show n 
                              in (text txt # fontSizeL 0.1 <> circle 0.1) # href ("javascript:alert(\"Node " ++ txt ++ "\")")

-- | Produces a value of the type 'Node'. The corrisponding diagram for the node is proudced by the @ (a -> Diagram B) @ function if the value contained in the 'Either' parameter is the 'Right' 'a' value.
-- The 'Either' parameter is 'Left' if the 'Node' is an <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Empty> node meaning that the 'Node''s name is the value of the 'c' accumulator from 'namesAndConnections' and its <https://hackage.haskell.org/package/diagrams Diagram> should be a blank node.
-- If the 'Either' parameter is instead 'Right', the 'Node''s 'name' is be produced using 'show' on the provided value and the associated diagram is produced from applying the provided Right value to the provided function.
getNode :: (Show a) => (a -> Diagram B) -> Either String a -> Node
getNode drawF x
    | isRight x = Node (show . head . rights $ [x]) ((drawF . head . rights $ [x]) # named (show . head . rights $ [x]))
    | otherwise = Node (fromLeft "" x) (drawDefaultEmptyNode 0.1 # named (fromLeft "" x))

-- | Essentially a wrapper for the recursive 'namesAndConnections' function. Takes a Diagram-producing function and a Graph of the same type the Diagram function takes and then produces a 'ProcessedGraph' containign all the vertices (their names and diagrams) and connections of the original graph.
getVertices :: (Eq a, Show a) => (a -> Diagram B)   -- ^ A function that takes a vertex of the graph of type 'a' and produces a <https://hackage.haskell.org/package/diagrams Diagram> from it.
                              -> Graph a            -- ^ The graph to be folded through.
                              -> ProcessedGraph     -- ^ Contains the produced 'Node''s and connections.
getVertices drawF g = namesAndConnections drawF g ""

-- | Recursively goes through the provided graph. Produces a list of 'Node''s (so the 'name' and associated <https://hackage.haskell.org/package/diagrams Diagram> for each vertex) and a list of connections between them.
namesAndConnections :: (Eq a, Show a) => (a -> Diagram B)   -- ^ A function that takes a vertex of the graph of type 'a' and produces a <https://hackage.haskell.org/package/diagrams Diagram> from it.
                                      -> Graph a            -- ^ The graph to be folded through.
                                      -> String             -- ^ An accumulator used to produce identifiers for <https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1/docs/Algebra-Graph.html#t:Graph Empty> vertices, each iteration an 'l' or 'r' is prepended.
                                      -> ProcessedGraph     -- ^ Contains the produced 'Node''s and connections.
namesAndConnections drawF Empty c = ProcessedGraph [getNode drawF (Left ("_empty_node_" ++ c))] []
namesAndConnections drawF v@(Vertex a) c = ProcessedGraph [getNode drawF (Right a)] []
namesAndConnections drawF (Overlay a b) c = ProcessedGraph (nA `union` nB) (cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections drawF a ('l' : c)
          (ProcessedGraph nB cB) = namesAndConnections drawF b ('r' : c)
namesAndConnections drawF (Connect a b) c = ProcessedGraph (nA `union` nB) ([(aA, bB) | aA <- nA, bB <- nB] `union` cA `union` cB)
    where (ProcessedGraph nA cA) = namesAndConnections drawF a ('l' : c)
          (ProcessedGraph nB cB) = namesAndConnections drawF b ('r' : c)

-- | Takes a list of connections and produces a corrisponding adjacency list (so tuples of a vertex and a list of vertices dependant on it) by using folds and recursion.
connectedTo :: (Eq a) => [(a,a)]        -- ^ A list of tuples where the first tuple element and second tuple element are used to corrispond to the vertex at the tail of the conenction and at the head of connection respectively.
                      -> ConnectList a  -- ^ The corrisponding adjacency list to the list of the connections.
connectedTo [] = []
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if not (null remaining) then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l

-- | Similar to 'connectedTo', takes a list of connections and produces a 'ConnectList' containing the vertices that each vertex depends on.
connectedFrom :: (Eq a) => [(a,a)] -> ConnectList a
connectedFrom [] = []
connectedFrom [(a,b),(x,y)]
  | b == y = [(b,[a,x])]
  | otherwise = (b,[a]) : [(y,[x])]
connectedFrom l@((x,y):zs) = (y,incoming) : if not (null remaining) then connectedFrom remaining else []
    where remaining = zs \\ filtered
          incoming = foldr (\(a,b) acc -> a : acc) [] filtered
          filtered = filter (\(a,b) -> b == y) l

-- | Used to produce a measurement for the size of <https://hackage.haskell.org/package/diagrams Diagram> properties from a default measurement and the size of a graph as an 'Integer'.
-- Multiplies the default value by 10 and divides this new value by the size of the graph.
dynamicStyle :: Measure Double -> Int -> Measure Double
dynamicStyle def graphSize = def * 10/fromIntegral graphSize