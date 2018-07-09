{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
import Data.List
import Algebra.Graph

data PGraph = PGraph [String] [Diagram B -> Diagram B]

data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         }

countV :: Graph a -> Int
countV (Vertex a) = 1
countV (Overlay a b) = countV a + countV b
countV (Connect a b) = countV a + countV b

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

-- node :: String -> Diagram B
node n = text n # fontSizeL 0.1 # href ("javascript:alert(\"Node " ++ n ++ "\")") <> circle 0.05 # named n

measureDiv :: Measure Double -> Measure Double -> Measure Double
measureDiv def graphSize = def * 10/graphSize

getVertices :: (Show a) => Settings -> Graph a -> PGraph
getVertices s (Vertex a) = PGraph [show a] []
getVertices s (Overlay a b) = PGraph (nA ++ nB) (cA ++ cB)
    where (PGraph nA cA) = getVertices s a
          (PGraph nB cB) = getVertices s b
getVertices s (Connect a b) = PGraph (nA ++ nB) (cA ++ cB ++ ((connectOutside' arrowOpts) <$> nA <*> nB))
    where (PGraph nA cA) = getVertices s a
          (PGraph nB cB) = getVertices s b
          arrowOpts = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)

visualise :: (Show a) => Settings -> Graph a -> Diagram B
visualise s g = mconcat $ connections <*> [diag]
    where diag = atPoints vertices $ node <$> names
          vertices = trailVertices layout
          layout   = layoutPoly $ countV g
          names = nub namesWDup
          (PGraph namesWDup connections) = getVertices s g

main = mainWith $ visualise (Settings (measureDiv normal $ fromIntegral (countV inputTestData)) (measureDiv thin $ fromIntegral (countV inputTestData))) inputTestData # frame 0.1

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 7) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))

