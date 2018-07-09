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

data PGraph = PGraph [String] [(String, String)] deriving (Show)

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

getVertices :: (Show a) => Graph a -> PGraph
getVertices (Vertex a) = PGraph [show a] []
getVertices (Overlay a b) = PGraph (nA ++ nB) (cA ++ cB)
    where (PGraph nA cA) = getVertices a
          (PGraph nB cB) = getVertices b
getVertices (Connect a b) = PGraph (nA ++ nB) ([(aA, bB) | aA <- nA, bB <- nB] ++ cA ++ cB)
    where (PGraph nA cA) = getVertices a
          (PGraph nB cB) = getVertices b


test :: (Eq a) => [(a,a)] -> [(a,Int)]
test [(a,b),(x,y)]
  | b == y = [(b,2)]
  | otherwise = (b,1) : [(y,1)]
test l@((x,y):ys) = (y,(length filtered)) : (test $ ys \\ filtered)
    where filtered = filter (\(a,b) -> b == y) l

visualise :: (Show a) => Settings -> Graph a -> Diagram B
visualise s g = mconcat $ zipWith (\a b -> a b diag) ((connectOutside' arrowOpts) <$> (fst <$> connections)) (snd <$> connections)
    where diag = atPoints vertices $ node <$> n
          vertices = trailVertices layout
          layout   = layoutPoly $ countV g
          n = nub names
          (PGraph names connections) = getVertices g
          arrowOpts = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)

main = mainWith $ visualise (Settings (measureDiv normal $ fromIntegral (countV inputTestData)) (measureDiv thin $ fromIntegral (countV inputTestData))) inputTestData # frame 0.1

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))

