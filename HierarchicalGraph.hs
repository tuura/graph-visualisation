{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

inputData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

node :: String -> Diagram B 
node n = text n # fontSizeL 0.3 <> circle 0.7 # named n

name :: Graph String -> String
name (Vertex a) = a
name (Overlay a b) = name a ++ "_overlay_" ++ name b
name (Connect a b) = name a ++ "_connect_" ++ name b

drawGraph :: Graph String -> Diagram B
drawGraph g@(Vertex a) = node $ name g
drawGraph g@(Overlay g1 g2) = (drawn <> boundingRect drawn # fc orange # lw none # opacity 0.4) # named (name g)
    where drawn = (drawGraph g1 === strutY 1 === drawGraph g2) # frame 0.2
drawGraph g@(Connect g1 g2) = (arrowed <> boundingRect arrowed # fc orange # lw none # opacity 0.4) # named (name g)
    where arrowed = connectOutside (name g1) (name g2) drawn
          drawn = (drawGraph g1 ||| strutX 1 ||| drawGraph g2) # frame 0.2

main = mainWith $ drawGraph inputData # frame 0.2