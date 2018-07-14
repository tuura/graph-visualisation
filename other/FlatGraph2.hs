{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Algebra.Graph

-- data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a) deriving (Show, Read)

data DrawnGraph a = DrawnGraph (Diagram B) [a]

node :: String -> Diagram B 
node n = text n # href ("javascript:alert(\"Node " ++ n ++ "\")") # fontSizeL 0.4 <> circle 0.7 # lwL 0.05 # named n

getVertices :: (Show a) => Graph a -> DrawnGraph String
getVertices g@(Vertex a) = DrawnGraph ((node $ show a)) [show a]
getVertices g@(Overlay a b) = DrawnGraph (aDiag ||| strutX 1 ||| bDiag) (aNames ++ bNames)
    -- where drawn1 = atPoints (trailVertices $ layoutPoly (countV a)) aDiag
    --       drawn2 = atPoints (trailVertices $ layoutPoly (countV b)) bDiag
    where (DrawnGraph aDiag aNames) = getVertices a
          (DrawnGraph bDiag bNames) = getVertices b
getVertices g@(Connect a b) = DrawnGraph (mconcat $ drawn : (connectOutside <$> aNames <*> bNames <*> [drawn])) (aNames ++ bNames)
    -- where drawn = atPoints (trailVertices $ layoutPoly (countV a + countV b)) (getVertices a ++ getVertices b)
    where drawn = aDiag Diagrams.Prelude.=== strutY 1 Diagrams.Prelude.=== bDiag
          connectionNum = if length aNames > length bNames then length aNames else length bNames
          -- drawn1 = atPoints (trailVertices $ layoutPoly (countV a)) aDiag
          -- drawn2 = atPoints (trailVertices $ layoutPoly (countV b)) bDiag
          (DrawnGraph aDiag aNames) = getVertices a
          (DrawnGraph bDiag bNames) = getVertices b

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

countV :: Graph a -> Int
countV (Vertex a) = 1
countV (Overlay a b) = countV a + countV b
countV (Connect a b) = countV a + countV b

-- getConnections :: (Show a) => Graph a -> [(a,a)]
-- getConnections (Vertex a) = (node $ show a) : []
-- getConnections (Connect g1 g2) = (countV g1 + countV g2)
-- getConnections (Connect g1 (Vertex b)) = (a,b) : []
-- getConnections (Overlay a b) = getVertices a ||| strutX 1 ||| getVertices b



main = mainWith $ d # frame 0.1
    where (DrawnGraph d _) = getVertices inputTestData

inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Connect (Connect (Vertex "1") (Connect (Connect (Overlay (Connect (Connect (Vertex "2") (Connect (Overlay (Vertex "3") (Vertex "4")) (Overlay (Connect (Vertex "5") (Vertex "6")) (Connect (Overlay (Vertex "7") (Vertex "8")) (Overlay (Vertex "9") (Connect (Vertex "10") (Connect (Vertex "11") (Vertex "12")))))))) (Overlay (Vertex "13") (Vertex "14"))) (Vertex "15")) (Vertex "16")) (Connect (Overlay (Vertex "17") (Overlay (Overlay (Vertex "18") (Vertex "19")) (Vertex "20"))) (Connect (Connect (Vertex "21") (Vertex "22")) (Vertex "23"))))) (Vertex "24"))

-- inputTestData = (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Overlay (Vertex 3) (Overlay (Vertex 4) (Connect (Connect (Vertex 5) (Connect (Overlay (Vertex 6) (Vertex 7)) (Vertex 8))) (Vertex 9)))))) (Vertex 10))
