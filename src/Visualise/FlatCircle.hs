{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Visualise.VisFlatCircle (
    drawFlatCircle
) where

import Algebra.Graph
import Visualise
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG         (renderSVG)
import Diagrams.Path
import Data.Char
import Data.List


data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         }

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

node :: String -> Diagram B
node n = text n # fontSizeL 0.1 # href ("javascript:alert(\"Node " ++ n ++ "\")") <> circle 0.05 # named n

visualiseFlatCircle :: (Show a) => Graph a -> Settings -> Diagram B
visualiseFlatCircle g s = mconcat $ connected
    where connected = map (\(a,b) -> connectOutside' arrowOpts a b diag) connections
          diag = atPoints vertices (node <$> names)
          vertices = trailVertices layout
          layout   = layoutPoly $ length names
          names = nub namesWDup
          (ProcessedGraph namesWDup connections) = getVertices graph
          graph = show <$> g
          arrowOpts = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)


drawFlatCircle :: (Show a) => FilePath -> (Maybe Double, Maybe Double) -> Graph a -> IO ()
drawFlatCircle path dim g = drawFlatCircle' defaultSettings path dim g

drawFlatCircle' :: (Show a) => Settings -> FilePath -> (Maybe Double, Maybe Double) -> Graph a -> IO ()
drawFlatCircle' s path (w,h) g = renderSVG path (mkSizeSpec2D w h) $ visualiseFlatCircle g s # frame 0.1

defaultSettings :: Settings
defaultSettings = Settings (dynamicStyle normal $ countVertices inputTestData) (dynamicStyle thin $ countVertices inputTestData)

-- main = mainWith $ visualiseFlatCircle () inputTestData # frame 0.1

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 7) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
inputTestData = (Connect (Vertex 2) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))

