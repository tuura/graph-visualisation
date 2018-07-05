{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

inputData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

colour :: Int -> Colour Double
colour 1 = orange
colour 2 = blue
colour 3 = red
colour _ = cyan

node :: String -> Diagram B 
node n = text n # fontSizeL 0.3 <> circle 0.7 # named n

name :: Graph String -> String
name (Vertex a) = a
name (Overlay a b) = name a ++ "_overlay_" ++ name b
name (Connect a b) = name a ++ "_connect_" ++ name b

draw :: Graph String -> Int -> (Int -> Colour Double) -> Diagram B
draw g@(Vertex a) l c = node $ name g
draw g@(Overlay g1 g2) l c = (drawn <> boundingRect drawn # fc (c l) # lw none # opacity 0.4) # named (name g)
    where drawn = (draw g1 (l + 1) c === strutY 1 === draw g2 (l + 1) c) # frame 0.2
draw g@(Connect g1 g2) l c = (arrowed <> boundingRect arrowed # fc (c l) # lw none # opacity 0.4) # named (name g)
    where arrowed = connectOutside (name g1) (name g2) drawn
          drawn = (draw g1 (l + 1) c ||| strutX 1 ||| draw g2 (l + 1) c) # frame 0.2

main = mainWith $ draw inputData 0 colour # frame 0.2