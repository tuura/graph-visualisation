{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Visualise.FlatCircle (
    Settings(..),

    drawFlatCircle, drawFlatCircle'
) where

import Algebra.Graph
import Visualise
import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG
import Diagrams.Path
import Data.Char
import Data.List


data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         }

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

drawFlatCircle :: (Show a, Eq a) => (a -> Diagram B) -> Graph a -> Diagram B
drawFlatCircle = drawFlatCircle' defaultSettings

drawFlatCircle' :: (Show a, Eq a) => (Graph a -> Settings) -> (a -> Diagram B) -> Graph a -> Diagram B
drawFlatCircle' settingsF drawF g = mconcat connected # frame 0.1
    where connected = map (\(a,b) -> connectOutside' arrowOpts (name a) (name b) noConnDiag) connections
          noConnDiag = atPoints vertices (diag <$> nodes)
          vertices = trailVertices layout
          layout   = layoutPoly $ countVertices g
          (ProcessedGraph nodes connections) = getVertices drawF g
          s = settingsF g
          arrowOpts = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)

defaultSettings :: Graph a -> Settings
defaultSettings g = Settings (dynamicStyle normal $ count g) (dynamicStyle thin $ count g)

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 7) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
inputTestData = (Connect (Vertex 2) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))

