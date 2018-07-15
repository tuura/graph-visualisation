{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Visualise.FlatAdaptive (
    Settings (..),

    drawFlatAdaptive
) where

import Visualise
import Algebra.Graph
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
import Data.List
import Data.Function


newtype Graphs a = Graphs [ProcessedGraph a] deriving (Show)

data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                     }

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 0.5

layoutVertices :: Int -> [Point V2 Double]
layoutVertices n = trailVertices $ regPoly n 0.5

node :: String -> Diagram B
node n = (text n # fontSizeL 0.1 # href ("javascript:alert(\"Node " ++ n ++ "\")") <> circle 0.1) # named n

connected :: (Show a, Eq a, Ord a) => [a] -> [(a,a)] -> ConnectList a
connected n l = groupConnected $ sortBy (\(x,_) (y,_) -> x `compare` y) (connectedTo l ++ connectedFrom l)

groupConnected :: (Show a, Eq a) => ConnectList a -> ConnectList a
groupConnected [(a,bs),(c,ds)]
    | a == c = [(a,bs++ds)]
    | otherwise = (a,bs):[(c,ds)]
groupConnected ((a,bs):(c,ds):es)
    | a == c = groupConnected ((a,nub $ bs++ds):es)
    | otherwise = (a,bs):groupConnected((c,ds):es)

smallConn :: (Show a, Eq a) => ConnectList a -> a -> Maybe (a,[a])
smallConn l n = find (\(x,y) -> x == n) l

oneConn :: (Show a, Eq a) => ConnectList a -> a -> Maybe (a,[a])
oneConn l n = find (\(x,y) -> x == n && length y == 1) l

getGroups :: (Show a, Eq a) => ConnectList a -> ConnectList a
getGroups = sortBy (flip compare `on` length . snd)

layoutGroups :: (Show a, Eq a, IsName a) => Diagram B -> [a] -> ConnectList a -> Diagram B
layoutGroups d n [] = d
layoutGroups d n groups@(x@(a,bs):xs)
    | null xs = makeGroup d x (n \\ (a:bs))
    | otherwise = layoutGroups (makeGroup d x (n \\ (a:bs))) n xs

makeGroup :: (Show a, Eq a, IsName a) => Diagram B -> (a,[a]) -> [a] -> Diagram B
makeGroup d c@(x,ys) others = withNames others (\otherSubs d -> 
        withNames (x:ys) (\subs@(s:ss) d -> moveTo (location s) (atPoints (layoutVertices . length $ subs) $ getSub <$> subs)) d) d
          
drawArrow :: Settings -> String -> String -> Diagram B -> Diagram B
drawArrow s a b d
    | a == b = connectPerim' arrowOpts2 a b (0 @@ turn) (-1/2 @@ turn) d
    | otherwise = connectOutside' arrowOpts1 a b d
    where arrowOpts1 = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)
          arrowOpts2 = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s) & arrowShaft .~ arc xDir (4/6 @@ turn)

initialPositions :: [String] -> Diagram B
initialPositions n = hsep 0.5 $ node <$> n

visualiseFlatAdaptive :: Settings -> Graph String -> Diagram B
visualiseFlatAdaptive s g = mconcat $ (\(a,b) -> drawArrow s a b outDiag) <$> connections
    where outDiag = overlayedDiagram ||| strutX 0.1 ||| connectedDiagram
          overlayedDiagram = overlayedOnlyDiagram names . listConnectedOnly $ connections
          connectedDiagram = connectedOnlyDiagram names connections . initialPositions $ names
          names = nub namesWDup
          (ProcessedGraph namesWDup connections) = getVertices g

connectedOnlyDiagram :: (Show a, IsName a) => [a] -> [(a,a)] -> Diagram B -> Diagram B
connectedOnlyDiagram names connections initialDiagram = layoutGroups initialDiagram names $ getGroups (connected names connections)

overlayedOnlyDiagram :: [String] -> [String] -> Diagram B
overlayedOnlyDiagram names connectedOnlyList = hsep 0.2 $ node <$> (names \\ connectedOnlyList)

listConnectedOnly :: (Show a, Eq a) => [(a,a)] -> [a]
listConnectedOnly connections = nub $ foldr (\(a,bs) acc -> a : bs ++ acc) [] (connectedTo connections ++ connectedFrom connections)

drawFlatAdaptive :: (Show a) => Settings -> FilePath -> Dimensions -> Graph a -> IO ()
drawFlatAdaptive s = drawFlatAdaptive' s

drawFlatAdaptive' :: (Show a) => Settings -> FilePath -> Dimensions -> Graph a -> IO ()
drawFlatAdaptive' s path dims g = draw path dims $ visualiseFlatAdaptive defaultSettings graphToString # frame 0.1
    where graphToString = show <$> g


defaultSettings :: Settings
defaultSettings = Settings (dynamicStyle normal $ countVertices inputTestData) 
                           (dynamicStyle thin $ countVertices inputTestData)


main = mainWith $ visualiseFlatAdaptive defaultSettings (show <$> inputTestData) # frame 0.1
-- main = mainWith $ visualiseFlatAdaptive () inputTestData # frame 0.1

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
-- inputTestData = (Connect (Vertex 2) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))
inputTestData = (Overlay (Vertex 50) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))
