{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

data Settings = Settings { colF :: (Int -> Colour Double)
                         , bgOp :: Double
                         }

node :: String -> Diagram B 
node n = text n # fontSizeL 0.3 <> circle 0.7 # named n

name :: Graph String -> String
name (Vertex a) = a
name (Overlay a b) = name a ++ "_overlay_" ++ name b
name (Connect a b) = name a ++ "_connect_" ++ name b

draw :: Graph String -> Int -> Settings -> Diagram B
draw g@(Vertex a) l s = node $ name g
draw g@(Overlay g1 g2) l s = (drawn <> boundingRect drawn # fc (colF s l) # lw none # opacity (bgOp s)) # named (name g)
    where drawn = (draw g1 (l + 1) s === strutY 1 === draw g2 (l + 1) s) # frame 0.2
draw g@(Connect g1 g2) l s = (arrowed <> boundingRect arrowed # fc (colF s l) # lw none # opacity (bgOp s)) # named (name g)
    where arrowed = connectOutside (name g1) (name g2) drawn
          drawn = (draw g1 (l + 1) s ||| strutX 1 ||| draw g2 (l + 1) s) # frame 0.2

main = mainWith $ draw inputTestData 0 (Settings colourTest 0.5) # frame 0.2


-- Test data:

inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

colourTest :: Int -> Colour Double
colourTest 0 = orange
colourTest 1 = red
colourTest _ = cyan