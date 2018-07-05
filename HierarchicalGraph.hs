{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

inputData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

node :: String -> Diagram B 
node n = text n # fontSizeL 0.1 <> circle 0.05 # named n

countGraphVertices :: Graph String -> Int
countGraphVertices (Vertex a) = 1
countGraphVertices (Overlay a b) = countGraphVertices a + countGraphVertices b
countGraphVertices (Connect a b) = countGraphVertices a + countGraphVertices b

name :: Graph String -> String
name (Vertex a) = a
name (Overlay a b) = name a ++ "_overlay_" ++ name b
name (Connect a b) = name a ++ "_connect_" ++ name b

drawGraph :: Graph String -> Diagram B
drawGraph g@(Vertex a) = node $ name g
drawGraph g@(Overlay g1 g2) = (drawGraph g1 === strutY 1 === drawGraph g2) # bgFrame 0.1 blue # opacity 0.5 # named (name g)
drawGraph g@(Connect g1 g2) = (connectOutside (name g1) (name g2) drawn) # bgFrame 0.1 orange # opacity 0.5 # named (name g)
    where drawn = drawGraph g1 ||| strutX 1 ||| drawGraph g2

visualise :: Graph String -> Diagram B
visualise g = drawGraph g # frame 0.2

main = mainWith $ visualise inputData