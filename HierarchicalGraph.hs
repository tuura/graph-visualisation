{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Graph a = Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a) deriving (Show)

data Settings = Settings { colF :: (Int -> Colour Double)
                         , bgOp :: Double
                         , dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         }

countV :: Graph String -> Int
countV (Vertex a) = 1
countV (Overlay a b) = countV a + countV b
countV (Connect a b) = countV a + countV b

node :: String -> Diagram B 
node n = text n # fontSizeL 0.4 <> circle 0.7 # lwL 0.05 # named n

name :: Graph String -> String
name (Vertex a) = a
name (Overlay a b) = name a ++ "_overlay_" ++ name b
name (Connect a b) = name a ++ "_connect_" ++ name b

draw :: Graph String -> Int -> Settings -> Diagram B
draw g@(Vertex a) l s = node $ name g
draw g@(Overlay g1 g2) l s = (drawn <> boundingRect drawn # fc (colF s l) # lw none # opacity (bgOp s)) # named (name g)
    where drawn = (draw g1 (l + 1) s === strutY 1 === draw g2 (l + 1) s) # frame 0.2
draw g@(Connect g1 g2) l s = (arrowed <> boundingRect arrowed # fc (colF s l) # lw none # opacity (bgOp s)) # named (name g)
    where arrowed = connectOutside' arrowOpts (name g1) (name g2) drawn
          drawn = (draw g1 (l + 1) s ||| strutX 1 ||| draw g2 (l + 1) s) # frame 0.2
          arrowOpts = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)

main = mainWith $ draw inputTestData 0 (Settings colourTest 0.9 (measureDiv normal entireSize) (measureDiv thin entireSize)) # frame 0.2
    where entireSize = countV inputTestData


-- Test data:

inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

-- inputTestData = (Connect (Connect (Overlay (Overlay (Vertex "0") (Overlay (Connect (Overlay (Connect (Overlay (Vertex "1") (Vertex "2")) (Overlay (Vertex "2") (Vertex "3"))) (Overlay (Vertex "2") (Vertex "3"))) (Overlay (Overlay (Connect (Connect (Vertex "2") (Vertex "3")) (Overlay (Connect (Vertex "3") (Vertex "4")) (Vertex "4"))) (Vertex "3")) (Connect (Vertex "3") (Vertex "4")))) (Overlay (Connect (Connect (Vertex "2") (Overlay (Connect (Vertex "3") (Vertex "4")) (Connect (Vertex "4") (Vertex "5")))) (Overlay (Connect (Connect (Overlay (Overlay (Vertex "3") (Vertex "4")) (Vertex "4")) (Connect (Vertex "4") (Overlay (Vertex "5") (Vertex "6")))) (Vertex "4")) (Connect (Connect (Vertex "4") (Vertex "5")) (Connect (Vertex "5") (Overlay (Vertex "6") (Overlay (Overlay (Vertex "7") (Vertex "8")) (Vertex "8"))))))) (Overlay (Connect (Connect (Vertex "3") (Overlay (Vertex "4") (Vertex "5"))) (Vertex "4")) (Vertex "4"))))) (Vertex "1")) (Connect (Vertex "1") (Vertex "2"))) (Vertex "1"))

-- inputTestData = (Overlay (Connect (Connect (Connect (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Connect (Connect (Overlay (Connect (Connect (Overlay (Overlay (Connect (Overlay (Connect (Overlay (Connect (Overlay (Connect (Overlay (Overlay (Connect (Connect (Overlay (Overlay (Connect (Connect (Connect (Connect (Overlay (Connect (Connect (Overlay (Overlay (Overlay (Overlay (Connect (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Overlay (Connect (Connect (Overlay (Overlay (Overlay (Overlay (Overlay (Connect (Connect (Connect (Connect (Overlay (Connect (Connect (Connect (Connect (Connect (Connect (Overlay (Overlay (Connect (Connect (Overlay (Overlay (Connect (Overlay (Connect (Connect (Connect (Connect (Connect (Connect (Connect (Overlay (Overlay (Connect (Connect (Connect (Connect (Overlay (Overlay (Overlay (Connect (Connect (Overlay (Overlay (Connect (Overlay (Connect (Overlay (Overlay (Overlay (Overlay (Connect (Overlay (Overlay (Overlay (Overlay (Overlay (Connect (Overlay (Connect (Connect (Connect (Overlay (Connect (Connect (Overlay (Overlay (Connect (Overlay (Overlay (Overlay (Overlay (Connect (Connect (Overlay (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Overlay (Overlay (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Connect (Overlay (Overlay (Connect (Connect (Overlay (Connect (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Connect (Overlay (Connect (Connect (Connect (Connect (Connect (Overlay (Connect (Connect (Connect (Overlay (Connect (Overlay (Connect (Overlay (Overlay (Connect (Connect (Connect (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Overlay (Connect (Overlay (Overlay (Connect (Connect (Connect (Overlay (Connect (Vertex "0") (Vertex "1")) (Vertex "2")) (Vertex "3")) (Vertex "4")) (Vertex "5")) (Vertex "6")) (Vertex "7")) (Vertex "8")) (Vertex "9")) (Vertex "10")) (Vertex "11")) (Vertex "12")) (Vertex "13")) (Vertex "14")) (Vertex "15")) (Vertex "16")) (Vertex "17")) (Vertex "18")) (Vertex "19")) (Vertex "20")) (Vertex "21")) (Vertex "22")) (Vertex "23")) (Vertex "24")) (Vertex "25")) (Vertex "26")) (Vertex "27")) (Vertex "28")) (Vertex "29")) (Vertex "30")) (Vertex "31")) (Vertex "32")) (Vertex "33")) (Vertex "34")) (Vertex "35")) (Vertex "36")) (Vertex "37")) (Vertex "38")) (Vertex "39")) (Vertex "40")) (Vertex "41")) (Vertex "42")) (Vertex "43")) (Vertex "44")) (Vertex "45")) (Vertex "46")) (Vertex "47")) (Vertex "48")) (Vertex "49")) (Vertex "50")) (Vertex "51")) (Vertex "52")) (Vertex "53")) (Vertex "54")) (Vertex "55")) (Vertex "56")) (Vertex "57")) (Vertex "58")) (Vertex "59")) (Vertex "60")) (Vertex "61")) (Vertex "62")) (Vertex "63")) (Vertex "64")) (Vertex "65")) (Vertex "66")) (Vertex "67")) (Vertex "68")) (Vertex "69")) (Vertex "70")) (Vertex "71")) (Vertex "72")) (Vertex "73")) (Vertex "74")) (Vertex "75")) (Vertex "76")) (Vertex "77")) (Vertex "78")) (Vertex "79")) (Vertex "80")) (Vertex "81")) (Vertex "82")) (Vertex "83")) (Vertex "84")) (Vertex "85")) (Vertex "86")) (Vertex "87")) (Vertex "88")) (Vertex "89")) (Vertex "90")) (Vertex "91")) (Vertex "92")) (Vertex "93")) (Vertex "94")) (Vertex "95")) (Vertex "96")) (Vertex "97")) (Vertex "98")) (Vertex "99")) (Vertex "100")) (Vertex "101")) (Vertex "102")) (Vertex "103")) (Vertex "104")) (Vertex "105")) (Vertex "106")) (Vertex "107")) (Vertex "108")) (Vertex "109")) (Vertex "110")) (Vertex "111")) (Vertex "112")) (Vertex "113")) (Vertex "114")) (Vertex "115")) (Vertex "116")) (Vertex "117")) (Vertex "118")) (Vertex "119")) (Vertex "120")) (Vertex "121")) (Vertex "122")) (Vertex "123")) (Vertex "124")) (Vertex "125")) (Vertex "126")) (Vertex "127")) (Vertex "128")) (Vertex "129")) (Vertex "130")) (Vertex "131")) (Vertex "132")) (Vertex "133")) (Vertex "134")) (Vertex "135")) (Vertex "136")) (Vertex "137")) (Vertex "138")) (Vertex "139")) (Vertex "140")) (Vertex "141")) (Vertex "142")) (Vertex "143")) (Vertex "144")) (Vertex "145")) (Vertex "146")) (Vertex "147")) (Vertex "148")) (Vertex "149")) (Vertex "150")) (Vertex "151")) (Vertex "152")) (Vertex "153")) (Vertex "154")) (Vertex "155")) (Vertex "156")) (Vertex "157")) (Vertex "158")) (Vertex "159")) (Vertex "160")) (Vertex "161")) (Vertex "162")) (Vertex "163")) (Vertex "164")) (Vertex "165")) (Vertex "166")) (Vertex "167")) (Vertex "168")) (Vertex "169")) (Vertex "170")) (Vertex "171")) (Vertex "172")) (Vertex "173")) (Vertex "174")) (Vertex "175")) (Vertex "176")) (Vertex "177")) (Vertex "178")) (Vertex "179")) (Vertex "180")) (Vertex "181")) (Vertex "182")) (Vertex "183")) (Vertex "184")) (Vertex "185")) (Vertex "186")) (Vertex "187")) (Vertex "188")) (Vertex "189")) (Vertex "190")) (Vertex "191")) (Vertex "192")) (Vertex "193")) (Vertex "194")) (Vertex "195")) (Vertex "196")) (Vertex "197")) (Vertex "198")) (Vertex "199")) (Vertex "200"))

colourTest :: Int -> Colour Double
colourTest i
    | odd i = red
    | otherwise = cyan

measureDiv :: Measure Double -> Int -> Measure Double
measureDiv def graphSize
    | graphSize < 10 = def
    | graphSize < 20 = def * (3/4)
    | graphSize < 30 = def * (1/2)
    | graphSize < 50 = def * (1/4)
    | otherwise = def * (1/8)