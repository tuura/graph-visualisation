{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Path

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

data Group = Group String [Diagram B]-- Group Name Diagrams
-- data Grouped = Single String (Diagram B) | Group [Grouped] (Maybe (Diagram B))


inputData = Overlay (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

getGraphName :: Graph a -> a
getGraphName (Vertex a) = a
-- getGraphName (Connect a b) = getGraphName a ++ getGraphName b
-- getGraphName (Overlay a b) = getGraphName a ++ getGraphName b


countGraphVertices :: Graph a -> Int
countGraphVertices (Vertex a) = 1
countGraphVertices (Overlay a b) = countGraphVertices a + countGraphVertices b
countGraphVertices (Connect a b) = countGraphVertices a + countGraphVertices b


graphPoly :: (V t ~ V2, TrailLike t) => Int -> t
graphPoly n = regPoly n 1

node :: String -> String -> Group
node l n = Group l (text l # fontSizeL 0.1 <> circle 0.05 # named l) : []

drawGraphConnect :: Group -> [(String,String)]
drawGraphConnect (Group _ _ (x:xs) a) = a
drawGraphConnect (Single _ _) = []


-- drawGraphConnect :: Grouped -> Grouped -> Grouped
-- drawGraphConnect a@(Single n1 d1) b@(Single n2 d2) = (Group (n1 ++ n2) (connect n1 n2 $ getOverlayDiagram a b) (a:b:[]) )
-- drawGraphConnect a@(Single n1 d1) b@(Group n2 d2 g2) = Group (n1 ++ n2) (connect n1 n2 $ getOverlayDiagram a b) (a : g2)
-- drawGraphConnect a@(Group n1 d1 g2) b@(Single n2 d2) = drawGraphConnect b a
-- drawGraphConnect a@(Group n1 d1 g1) b@(Group n2 d2 g2) = Group (n1 ++ n2) (connect n1 n2 $ getOverlayDiagram a b) (g1 ++ g2)

drawGraphConnect :: Group -> Group -> Group
drawGraphConnect a@(Group n1 d1) b@(Group n2 d2) = Group (n1 ++ n2) (getOverlayDiagram a b)

-- drawGraphConnect :: Grouped -> Grouped -> Grouped
-- drawGraphConnect a b = Group name diagram items True
--     where (Group name diagram items _) = drawGraphOverlay a b

getOverlayDiagram :: Group -> Group -> [Diagram B]
getOverlayDiagram a b = overlayed
    where (Group _ overlayed _ _) = drawGraphOverlay a b

drawGraphOverlay :: Group -> Group -> Group
-- drawGraphOverlay a@(Single n1 d1) b@(Single n2 d2) = Group (n1 ++ n2) (a:b:[]) $ Just (hsep 1 [d1, d2])

drawGraphOverlay a@(Group n1 (d1:[]) b@(Group n2 d2) = Group (n1 ++ n2) (d1 ++ d2)
drawGraphOverlay a@(Group n1 d1) b@(Group n2 d2) = Group (n1 ++ n2) (d1 ++ d2)


gCn (Group ) g2 = atPoints vertices1 output1
    where vertices1 = trailVertices . graphPoly . countGraphVertices $ g1
          groups@(Group _ output1 _ _) = (getVertices g)


makeVertex :: Graph String -> Group 
makeVertex (Vertex a) = node a a

getVertices :: Graph String -> Group
getVertices v@(Vertex a) = makeVertex v
getVertices (Connect (Vertex a) (Vertex a)) = connect 
getVertices g@(Connect a b) = prepGraphConnect (getVertices a) (getVertices b)
    where vertices = trailVertices . graphPoly . countGraphVertices $ g
          ()
getVertices (Overlay a b) = drawGraphOverlay (getVertices a) (getVertices b)



visualise :: Graph String -> Diagram B
visualise input = atPoints vertices outputD
    where vertices = trailVertices . graphPoly . countGraphVertices $ input
          groups@(Group _ outputD _ _) = (getVertices input)
-- visualise input = atPoints vertices $ map node 
--     where vertices = trailVertices graphPoly $ countGraphVertices input

main = mainWith $ visualise inputData