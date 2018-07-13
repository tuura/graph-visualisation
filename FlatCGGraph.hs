{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Diagrams.Prelude hiding (union)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
import Data.List
import Data.Function
import Algebra.Graph hiding ((===))

data PGraph a = PGraph [a] [(a, a)] deriving (Show)

type ConnectList a = [(a,[a])]

getVertices :: (Show a, Eq a) => Graph a -> PGraph a
getVertices (Vertex a) = PGraph [a] []
getVertices (Overlay a b) = PGraph (nA ++ nB) (cA ++ cB)
    where (PGraph nA cA) = getVertices a
          (PGraph nB cB) = getVertices b
getVertices (Connect a b) = PGraph (nA ++ nB) ([(aA, bB) | aA <- nA, bB <- nB] ++ cA ++ cB)
    where (PGraph nA cA) = getVertices a
          (PGraph nB cB) = getVertices b

connectedFrom :: (Show a, Eq a) => [(a,a)] -> ConnectList a
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

connectedTo :: (Show a, Eq a) => [(a,a)] -> ConnectList a
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if length remaining > 0 then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l

reduction :: (Show a, Eq a) => ConnectList a -> ConnectList a -> ConnectList a
reduction [] _ = []
reduction ((x,ys):zs) extras = new : reduction zs (new : extras)
    where new = (x,nub $ foldr (\y acc -> check y acc) ys ys)
          check y accOuter = foldr (\(a,bs) acc -> if y == a then acc \\ (acc `intersect` bs) else acc) accOuter toCheck
          toCheck = extras ++ zs


getRoots :: (Show a, Eq a) => [a] -> ConnectList a -> [a]
getRoots n reduced = foldr (\(x,ys) acc -> delete x acc) n reduced

-- -- Topological sorting
-- getLevelList :: [String] -> [(String,[String])] -> [String]
-- getLevelList n connTo = fst $ 
--                             foldr (\a (acc,cTo) -> 
--                             let new = (a : acc) -- Prepend the current root node to the list
--                                 folded = foldConnected a new cTo -- Fold over the nodes connected to the root node
--                             in ((fst folded) ++ new, snd folded)) -- Add the nodes which have no other connections currently to the list
--                             ([],connTo) (roots connTo) -- Fold over the root nodes with no incoming connections
--     where foldConnected a new cTo = foldr (\b (acc1,acc2) -> -- b is the current node connected to root node a, acc1 is the current list, acc2 is the current adjacency list
--                                       -- if countIncomingXC b acc2 == 0 then (b : acc1, newConnectedTo a b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
--                                       if length xConnectedTo b acc2 == 0 then (b : acc1, newConnectedTo a b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
--                                       else (acc1, newConnectedTo a b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
--                                     (new,cTo) (xConnectedTo a cTo)
--           newConnectedTo x r cTo = (x,(delete r $ xConnectedTo x cTo)) : -- Deletes the processed edge from the adjacency list and builds a tuple from it
--                                    (notXConnections $ findXToConnections x cTo) -- Adds the modified tuple to the rest of the adjacency list
--           xConnectedTo x cTo = xConnections $ findXToConnections x cTo -- Gets a list of nodes connected to the node x
--           notXConnections (xConn,nXConn) = nXConn -- Gets the rest of the adjacency list without the node x element
--           xConnections (xConn,nXConn) = snd . head $ xConn -- Gets a list of nodes connected to node x, there is only one element (hence head) and the list is in snd of the tuple
--           -- xConnections ((x:xs),nXConn) = snd x -- Gets a list of nodes connected to node x, there is only one element (hence head) and the list is in snd of the tuple
--           findXToConnections x cTo = partition (\(a,bs) -> a == x) cTo -- Finds the element in the adjacency list for node x and gives it and the rest in a tuple
--           countIncomingXC x cTo = foldr (\(a,bs) acc -> if x `elem` bs then acc + 1 else acc) 0 cTo -- Counts the number of incoming edges to node x
          
--           getEdgesFrom x cTo = foldr (\(a,bs) acc -> if x `elem` bs then a : acc else acc) [] cTo

--           roots cTo = getRoots n cTo -- Gets the root nodes from the adjacency list

-- Topological sorting
getLevelList :: (Show a, Eq a) => [a] -> ConnectList a -> [a]
getLevelList [] _ = []
getLevelList q firstCTo = let (queue,list,cTo) = foldThroughRoots q firstCTo
                              in list ++ (getLevelList queue cTo)
    -- where recurse = (Just (fst foldRoots)) `mappend` (getLevelList (n \\ roots connTo) $ snd foldRoots)
          
foldThroughRoots :: (Show a, Eq a) => [a] -> ConnectList a -> ([a], [a], ConnectList a)
foldThroughRoots queue firstCTo = foldr (\root (accQueue,accList,accCTo) -> 
                                    let newList = (accList ++ [root]) -- Prepend the current root node to the list
                                        folded = foldThroughConnectedNodes root accQueue accCTo -- Fold over the nodes connected to the root node
                                    in (fst folded, newList, snd folded)) -- Add the nodes which have no other connections currently to the list
                                    ([],[],firstCTo) queue -- Start by folding over the root nodes with no incoming connections


foldThroughConnectedNodes :: (Show a, Eq a) => a -> [a] -> ConnectList a -> ([a], ConnectList a)
foldThroughConnectedNodes root queue cTo = foldr (\b (accQueue,acc2) -> -- b is the current node connected to root node a, acc1 is the current list, acc2 is the current adjacency list
                                              if length (getEdgesTo b acc2) <= 1 then (accQueue ++ [b], deleteConnection root b acc2) -- If the node b has no more incoming edges it is added to the list and removed from the adjacency list
                                              else (accQueue, deleteConnection root b acc2)) -- Otherwise the list remains the same but it is still removed from the adjacency list
                                              (queue,cTo) (getEdgesFrom root cTo)

deleteConnection :: (Show a, Eq a) => a -> a -> ConnectList a -> ConnectList a
deleteConnection x r cTo = (\(a,bs) -> (a,delete x bs)) <$> cTo

-- deleteConnection x r cTo = (x,delete r $ getEdgesFrom x cTo) : 
-- if getEdgesToTuple x cTo /= Nothing then 
--   let (Just tup) = getEdgesToTuple x cTo 
--       in (cTo \\ [tup])
-- else []

dependsOn :: (Show a, Eq a) => a -> a -> ConnectList a -> Bool
dependsOn a b cTo = b `elem` (getEdgesTo a cTo) -- If a depends on b

getEdgesTo :: (Show a, Eq a) => a -> ConnectList a -> [a]
getEdgesTo x cTo = if getEdgesToTuple x cTo /= Nothing then let (Just tup) = getEdgesToTuple x cTo in snd tup else []

getEdgesToTuple :: (Show a, Eq a) => a -> ConnectList a -> Maybe (a,[a])
getEdgesToTuple x cTo = find (\(a,bs) -> a == x) cTo

getEdgesFrom :: (Show a, Eq a) => a -> ConnectList a -> [a]
getEdgesFrom x cTo = foldr (\(a,bs) acc -> if x `elem` bs then a : acc else acc) [] cTo

-- getLevels :: (Show a, Eq a) => [a] -> [(a,[a])] -> [[a]]
getLevels [] _ = []
getLevels l cTo = foldLevel : (getLevels (l \\ foldLevel) cTo)
    where foldLevel = fst $ foldr (\x (acc,flag) -> if flag || (or $ zipWith (x `dependsOn`) acc (repeat cTo)) || (or $ map (\a -> dependsOn a x cTo) acc) then (acc,True) else (x:acc,False)) ([],False) l

reducedConnections :: (Show a, Eq a) => ConnectList a -> [(a,a)]
reducedConnections l = foldr (\(x,ys) acc -> (zip ys (repeat x)) ++ acc) [] l

node :: String -> Diagram B
node n = (text n # fontSizeL 0.1 # href ("javascript:alert(\"Node " ++ n ++ "\")") <> circle 0.1) # named n

layerDifference :: (Show a, Eq a) => a -> a -> [[a]] -> Int
layerDifference a b l = (foldLayers a) - (foldLayers b)
    where foldLayers x = snd (foldr (\xs (c1,c2) -> if x `elem` xs then (c1,c1) else (c1 + 1,c2)) (0,0) l)

visualiseLayers :: [[String]] -> Diagram B
visualiseLayers levelled = vsep 0.2 $ foldl (\acc level -> (center (hsep 0.3 $ node <$> level)) : acc) [] levelled

visualise :: Diagram B
visualise = mconcat connected
    -- where connected = zipWith (\a b -> (if (layerDifference a b levelled) > 1 then arrows arrowOpts2 a b else arrows arrowOpts1 a b)) (fst <$> finalConnections) (snd <$> finalConnections)
    where connected = map (\(a,b) -> (if abs (layerDifference a b levelled) > 1 then arrows arrowOpts2 a b else arrows arrowOpts1 a b)) finalConnections
          arrows aO a b = connectPerim' aO a b (-1/4 @@ turn) (1/4 @@ turn) $ visualiseLayers levelled
          arrowOpts1 = with
          arrowOpts2 = with & arrowShaft .~ (arc xDir (3/12 @@ turn))
          finalConnections = reducedConnections reduced
          levelled = getLevels topList reduced 
          -- topList :: (Show a, Eq a) => [a]
          topList = getLevelList (getRoots n reduced) reduced
          reduced = reduction (connectedFrom connections) []
          -- n :: (Show a, Eq a) => [a]
          n = nub names


          (PGraph names connections) = getVertices g
          g = inputTestData

main = mainWith $ visualise # frame 0.1

-- visualise :: Diagram B
-- visualise g = reduction $ connectedFrom connections
--     where (PGraph names connections) = getVertices g

-- main = mainWith $ visualise inputTestData # frame 0.1

-- inputTestData = (Connect (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 21))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 15)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
inputTestData :: Graph String
inputTestData = fmap show $ Connect (Vertex 1) (Overlay (Connect (Vertex 2) (Overlay (Connect (Vertex 4) (Vertex 7)) (Connect (Vertex 5) (Vertex 7)))) (Connect (Vertex 3) (Connect (Vertex 6) (Connect (Vertex 5) (Vertex 7)))))
