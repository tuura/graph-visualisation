{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path
import Data.Char
import Data.List
import Data.Function
import Algebra.Graph hiding ((===))

data PGraph = PGraph [String] [(String, String)] deriving (Show)

data Graphs = Graphs [PGraph] deriving (Show)

data Settings = Settings { dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                     }

countV :: Graph a -> Int
countV (Vertex a) = 1
countV (Overlay a b) = countV a + countV b
countV (Connect a b) = countV a + countV b

layoutPoly :: (V t ~ V2, TrailLike t) => Int -> t
layoutPoly n = regPoly n 0.5

node :: String -> Diagram B
node n = (text n # fontSizeL 0.1 # href ("javascript:alert(\"Node " ++ n ++ "\")") <> circle 0.1) # named n

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



connectedFrom :: [(String,String)] -> [(String,[String])]
connectedFrom [(a,b),(x,y)]
  | b == y = [(b,[a,x])]
  | otherwise = (b,[a]) : [(y,[x])]
connectedFrom l@((x,y):zs) = (y,incoming) : if length remaining > 0 then connectedFrom remaining else []
    where remaining = zs \\ filtered
          incoming = foldr (\(a,b) acc -> a : acc) [] filtered
          filtered = filter (\(a,b) -> b == y) l

connectedTo :: [(String,String)] -> [(String,[String])]
connectedTo [(a,b),(x,y)]
  | a == x = [(a,[b,y])]
  | otherwise = (a,[b]) : [(x,[y])]
connectedTo l@((x,y):zs) = (x,outgoing) : if length remaining > 0 then connectedTo remaining else []
    where remaining = zs \\ filtered
          outgoing = foldr (\(a,b) acc -> b : acc) [] filtered
          filtered = filter (\(a,b) -> a == x) l

connected :: [String] -> [(String,String)] -> [(String,[String])]
connected n l = groupConnected $ sortBy (\(x,_) (y,_) -> x `compare` y) (connectedTo l ++ connectedFrom l)

groupConnected ((a,bs):(c,ds):[])
    | a == c = [(a,bs++ds)]
    | otherwise = (a,bs):[(c,ds)]

groupConnected ((a,bs):(c,ds):es)
    | a == c = groupConnected ((a,nub $ bs++ds):es)
    | otherwise = (a,bs):groupConnected((c,ds):es)
-- connected l = concat $ (\(a,b) (c,d) -> if a == c then [(a,b++d)] else [(a,b),(c,d)]) <$> connectedFrom l <*> connectedTo l



-- test1 :: (Eq a) => [(a,[a])] -> [(a,[a])]
-- test1 l = filter (\(x,y) -> length y == 1) l

smallConn :: [(String,[String])] -> String -> Maybe (String,[String])
smallConn l n = find (\(x,y) -> x == n) l

oneConn :: [(String,[String])] -> String -> Maybe (String,[String])
oneConn l n = find (\(x,y) -> x == n && length y == 1) l

-- nodes :: [(String,[String])] -> [String] -> [Diagram B]
-- nodes _ [] = []
-- nodes isConnectedFrom all@(n:ns)
--     | isSmall == Nothing = node n : nodes isConnectedFrom ns
--     | otherwise = let (Just iS@(x,y:[])) = isSmall in atPoint (withName y ) node x) : nodes (delete iS isConnectedFrom) (delete x (delete y ns))
--         where isSmall = smallConn isConnectedFrom n

-- newLayout :: Diagram B -> [String] -> [(String,[String])] -> Diagram B
-- newLayout d [] cF = d
-- -- newLayout d (n:[]) cF
-- --     | isSmall == Nothing = withName n (\sD d -> getSub sD) d
-- --     | otherwise = test1 d n cF isSmall
-- --         where isSmall = smallConn cF n
-- newLayout d (n:ns) cF
--     -- | hasConn == Nothing = withName n (\sD d -> getSub sD) d <> test d ns cF <> (if hasOneConn /= Nothing then test1 d n cF hasOneConn else mempty)
--     | hasConn == Nothing = newLayout d ns cF -- <> (if hasOneConn /= Nothing then test1 d n cF hasOneConn else withName n (\sD d -> getSub sD) d) 
--     | otherwise = let (Just (x,ys)) = hasConn in newLayout newDiag (ns \\ (x:ys)) cF
--         where newDiag = makeGroup d hasConn
--               hasConn = smallConn cF n
--               -- hasOneConn = oneConn cF n
--               -- layoutOne = if hasOneConn /=

-- deleteAll :: [String] -> [String] -> [String]
-- deleteAll Nothing ns = ns
-- deleteAll (Just (x,y:ys))

getGroups :: [(String,[String])] -> [(String,[String])]
getGroups cF = sortBy (flip compare `on` length . snd) cF

layoutGroups :: Diagram B -> [String] -> [(String,[String])] -> Diagram B
layoutGroups d n [] = d
layoutGroups d n groups@(x@(a,bs):xs)
    | xs == [] = makeGroup d x (n \\ (a:bs))
    | otherwise = layoutGroups (makeGroup d x (n \\ (a:bs))) n (xs)

-- layoutGroups d groups@(x:xs) = layoutGroups (makeGroup d (Just x)) (xs)
-- layoutGroups d g = foldr (\x acc -> makeGroup acc (Just x)) d g
-- layoutGroups d groups@(w:x:y:z:zs) = (makeGroup (makeGroup d (Just w)) (Just x))

-- test1 d n cF (Just c@(x,y:[])) = withNames [x,y] (\[sD1,sD2] d -> beside (r2 (1,0)) (rawSub sD1) (rawSub sD2)) d -- TODO:Fix here
makeGroup :: Diagram B -> (String,[String]) -> [String] -> Diagram B
makeGroup d c@(x,ys) others = withNames others (\otherSubs d -> 
        withNames (x:ys) (\subs@(s:ss) d -> moveTo (location s) (atPoints (trailVertices (layoutPoly $ length subs)) $ getSub <$> subs)) d) d
        -- withNames (x:ys) (\subs@(s:ss) d -> (mconcat $ getSub <$> otherSubs) <> moveTo (location s) (atPoints (trailVertices (layoutPoly $ length ss)) $ getSub <$> ss)) d) d
        -- withNames (x:ys) (\subs@(s:ss) d -> otherSubs <> moveTo (location s) (atPoints (trailVertices (layoutPoly $ length ss)) $ zipWith (\a b -> b # named a) ys (rawSub <$> ss))) d) d
  --moveTo (translateX 1 $ location sD2) (rawSub sD1)) d -- TODO:Fix here

visualise :: (Show a) => Settings -> Graph a -> Diagram B
-- visualise s g = mconcat $ zipWith (\a b -> a b testDiag) ((connectOutside' arrowOpts) <$> (fst <$> connections)) (snd <$> connections)
visualise s g = mconcat $ (\(a,b) -> drawArrow s a b outDiag) <$> (zip (fst <$> connections) (snd <$> connections))
-- visualise s g = testDiag
    where outDiag = unconnectedOnly ||| strutX 0.1 ||| connectedOnly
          unconnectedOnly = hsep 0.2 $ node <$> (n \\ rawConnected)
          connectedOnly = withNames rawConnected (\sDs d -> mconcat $ getSub <$> sDs) groupDiag
          rawConnected = nub $ foldr (\(a,bs) acc -> a : bs ++ acc) [] (connectedTo connections ++ connectedFrom connections)
          groupDiag = (layoutGroups diag n $ getGroups (connected n connections))
          -- testDiag = newLayout diag n $ connected connections
          -- diag = atPoints vertices $ node <$> n
          diag = hsep 0.5 $ node <$> n
          -- diag = initialPlacement $ node <$> n
          vertices = trailVertices layout
          layout   = layoutPoly $ length n
          n = nub $ names
          (PGraph names connections) = getVertices g
          
drawArrow s a b d
    | a == b = connectPerim' arrowOpts2 a b (0 @@ turn) (-1/2 @@ turn) d
    | otherwise = connectOutside' arrowOpts1 a b d
    where arrowOpts1 = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)
          arrowOpts2 = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s) & arrowShaft .~ (arc xDir (4/6 @@ turn))

initialPlacement (n:[]) = n
initialPlacement (n:ns) = n === strutY 1 === initialPlacement ns

main = mainWith $ visualise (Settings (measureDiv normal $ fromIntegral (countV inputTestData)) (measureDiv thin $ fromIntegral (countV inputTestData))) inputTestData # frame 0.1

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))
-- inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))
-- inputTestData = (Connect (Vertex 2) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))
inputTestData = (Overlay (Vertex 50) (Overlay (Connect (Vertex 5) (Connect (Vertex 4) (Overlay (Vertex 8) (Connect (Connect (Vertex 12) (Connect (Vertex 6) (Vertex 4))) (Overlay (Connect (Vertex 14) (Connect (Vertex 6) (Connect (Vertex 11) (Vertex 11)))) (Vertex 2)))))) (Overlay (Vertex 9) (Overlay (Vertex 22) (Connect (Vertex 11) (Overlay (Vertex 2) (Connect (Overlay (Connect (Overlay (Vertex 1) (Vertex 8)) (Connect (Vertex 33) (Vertex 9))) (Connect (Vertex 39) (Vertex 30))) (Connect (Vertex 27) (Vertex 29)))))))))
