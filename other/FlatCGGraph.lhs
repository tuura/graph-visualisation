{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude hiding (union)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
import Data.List
import Data.Function
import Algebra.Graph hiding ((===))

data PGraph = PGraph [String] [(String, String)] deriving (Show)

getVertices :: (Show a) => Graph a -> PGraph
getVertices (Vertex a) = PGraph [show a] []
getVertices (Overlay a b) = PGraph (nA ++ nB) (cA ++ cB)
    where (PGraph nA cA) = getVertices a
          (PGraph nB cB) = getVertices b
getVertices (Connect a b) = PGraph (nA ++ nB) ([(aA, bB) | aA <- nA, bB <- nB] ++ cA ++ cB)
    where (PGraph nA cA) = getVertices a
          (PGraph nB cB) = getVertices b

connectedFrom :: [(String,String)] -> [(String,[String])]
connectedFrom [(a,b),(x,y)]
  | b == y = [(b,[a,x])]
  | otherwise = (b,[a]) : [(y,[x])]
connectedFrom l@((x,y):zs) = (y,incoming) : if length remaining > 0 then connectedFrom remaining else []
    where remaining = zs \\ filtered
          incoming = foldr (\(a,b) acc -> a : acc) [] filtered
          filtered = filter (\(a,b) -> b == y) l

-- reducedFrom :: [(String,[String])] -> [(String,[String])]
-- reducedFrom ((x1,ys1):zs1) = foldr (\a acc -> x1 : ) [] ys1
--     where getCFrom ((x,ys):zs) = foldr (\a acc -> )

connectedTo :: [(String,String)] -> [(String,[String])]
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if length remaining > 0 then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l

reduction :: [(String,[String])] -> [(String,[String])] -> [(String,[String])]
reduction [] _ = []
reduction ((x,ys):zs) extras = new : reduction zs (new : extras)
    where new = (x,foldr (\y acc -> check y acc) ys ys)
          check y accOuter = foldr (\(a,bs) acc -> if y == a then acc \\ (acc `intersect` bs) else acc) accOuter toCheck
          toCheck = extras ++ zs


getRoots :: [String] -> [(String,[String])] -> [String]
getRoots n reduced = foldr (\(x,ys) acc -> delete x acc) n reduced

getLevelList :: [String] -> [(String,[String])] -> [String]
getLevelList n connTo = fst $ 
                            foldr (\a (acc,cTo) -> 
                            let new = (a : acc) 
                                folded = nTo a new cTo
                            in ((fst folded) ++ new, snd folded)) 
                            ([],connTo) (roots connTo)
    where nTo a new cTo = foldr (\b (acc1,acc2) -> 
                          if countIncomingXC b acc1 == 0 then (b : acc1, newConnectedTo a b acc2) 
                            else (acc1, newConnectedTo a b acc2)) 
                          (new,cTo) (xConnectedTo a cTo)
          newConnectedTo x r cTo = (x,(delete r $ xConnectedTo x cTo)) : 
                                   (notXConnections $ findXToConnections x cTo)
          xConnectedTo x cTo = xConnections $ findXToConnections x cTo
          notXConnections (xConn,nXConn) = nXConn
          xConnections (xConn,nXConn) = snd . head $ xConn
          findXToConnections x cTo = partition (\(a,bs) -> a == x) cTo
          -- remCFrom x r cs otherCFrom = (x,(delete r cs)) : otherCFrom
          countIncomingXC x cTo = foldr (\(a,bs) acc -> if x `elem` bs then acc + 1 else acc) 0 cTo
          roots cTo = getRoots n cTo

test = getLevelList n reduced
    where reduced = reduction (connectedFrom connections) []
          n = nub names
          (PGraph names connections) = getVertices g
          g = inputTestData
-- visualise :: Diagram B
-- visualise g = reduction $ connectedFrom connections
--     where (PGraph names connections) = getVertices g

-- main = mainWith $ visualise inputTestData # frame 0.1

inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
