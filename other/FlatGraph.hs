{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char

connections :: [[Char]]
connections = [['b','c','d','e'],['d','e'],['d','e'],['e'],[]]

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 1

node :: Char -> Diagram B
node l = text [l] # fontSizeL 0.1 <> circle 0.05

nodeIntToPoint :: [Point v n] -> Int -> Int -> Point v n
nodeIntToPoint (x:xs) a n
    | a == n = x
    | otherwise = nodeIntToPoint xs a (n+1)

arrowPairsToPoints :: [Point v n] -> [(Char,Char)] -> [(Point v n, Point v n)]
arrowPairsToPoints vertices = foldr (\(x,y) acc -> (nodeIntToPoint vertices (asInt x) 0, nodeIntToPoint vertices (asInt y) 0) : acc) []
    where asInt a = (ord a) - 97

toArrowPairs :: [[Char]] -> [(Char,Char)]
toArrowPairs conns = foldr (\(l,xs) acc -> (foldr (\y acc2 -> (l,y) : acc2) acc xs)) [] $ zip ['a'..'z'] conns

visualiseArrows :: [(Point V2 Double, Point V2 Double)] -> [Diagram B]
visualiseArrows pairs = foldr (\(x,y) acc -> arrowBetween' arrowOpts x y : acc) [] pairs
    where arrowOpts = with & gaps .~ 12

main = mainWith $ mconcat (atPoints vertices (map node ['a'..'z']) : visualiseArrows (arrowPairsToPoints vertices $ toArrowPairs connections)) # frame 1
    where vertices = trailVertices layout
          layout   = layoutPoly $ length connections