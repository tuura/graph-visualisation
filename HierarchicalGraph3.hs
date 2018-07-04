{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

data Node = Node String (Diagram B)

inputData = Overlay (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))


node :: String -> String -> Node 
node l n = Node l ((text l # fontSizeL 0.1 <> circle 0.05 # named l) )

countGraphVertices :: Graph String -> Int
countGraphVertices (Vertex a) = 1
countGraphVertices (Overlay a b) = countGraphVertices a + countGraphVertices b
countGraphVertices (Connect a b) = countGraphVertices a + countGraphVertices b

draw :: Graph String -> Node
draw (Vertex a) = node a a
-- draw (Overlay a b) = Node (n1 ++ n2) (d1 ++ d2) -- TODO: Carry on here with # named
--     where (Node n1 d1) = draw a
--           (Node n2 d2) = draw b
draw (Overlay a b) = Node (n1 ++ n2) ((hsep 1 [d1, (d2)]) # named (n1 ++ n2))
    where (Node n1 d1) = draw a
          (Node n2 d2) = draw b
draw (Connect a b) = Node n3 ((connect n1 n2 d3) # bgFrame 0.1 orange)
    where (Node n1 d1) = draw a
          (Node n2 d2) = draw b
          (Node n3 d3) = draw (Overlay a b)
-- draw (Connect a b) = draw (Overlay a b)

visualise :: Graph String -> Diagram B
visualise g = d
    where Node _ d = draw g
-- visualise :: Graph String -> Diagram B
-- visualise g = hsep 1 d
--     where Node _ d = draw g

main = mainWith $ visualise inputData