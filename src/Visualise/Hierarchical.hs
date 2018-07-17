{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Visualise.Hierarchical (
    Settings(..),

    drawHierarchical, drawHierarchical'
) where

import Algebra.Graph hiding ((===))
import Visualise
import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG

data Settings = Settings { colF :: Int -> Colour Double
                         , bgOp :: Double
                         , dynamicHead :: Measure Double
                         , dynamicThick :: Measure Double
                         }

-- node :: String -> Diagram B 
-- node n = text n # href ("javascript:alert(\"Node " ++ n ++ "\")") # fontSizeL 0.4 <> circle 0.7 # lwL 0.05 # named n

name :: (Show a) => Graph a -> String
name (Vertex a) = show a
name (Overlay a b) = name a ++ "_overlay_" ++ name b
name (Connect a b) = name a ++ "_connect_" ++ name b

visualiseHier :: (Show a) => Graph a -> Int -> Settings -> Diagram B
visualiseHier g@(Vertex a) l s = (node 0.4 0.7 $ name g) # lwL 0.05
visualiseHier g@(Overlay g1 g2) l s = (drawn <> boundingRect drawn # fc (colF s l) # lw none # opacity (bgOp s)) # named (name g)
    where drawn = (visualiseHier g1 (l + 1) s === strutY 1 === visualiseHier g2 (l + 1) s) # frame 0.2
visualiseHier g@(Connect g1 g2) l s = (arrowed <> boundingRect arrowed # fc (colF s l) # lw none # opacity (bgOp s)) # named (name g)
    where arrowed = connectOutside' arrowOpts (name g1) (name g2) drawn
          drawn = (visualiseHier g1 (l + 1) s ||| strutX 1 ||| visualiseHier g2 (l + 1) s) # frame 0.2
          arrowOpts = with & headLength .~ dynamicHead s & shaftStyle %~ lw (dynamicThick s)

drawHier :: (Show a) => Graph a -> Diagram B
drawHier g = drawHier' (defaultSettings g) g

drawHier' :: (Show a) => Settings -> Graph a -> Diagram B
drawHier' s g = visualiseHier g 0 s # frame 0.1

defaultSettings :: Graph a -> Settings
defaultSettings g = Settings alternatingColour 1 (dynamicStyle normal entireSize) (dynamicStyle thin entireSize)
    where entireSize = countVertices g

alternatingColour :: Int -> Colour Double
alternatingColour i
    | odd i = red
    | otherwise = cyan


-- main = mainWith $ visualiseHier inputTestData 0 defaultSettings # frame 0.2

-- Test data:

-- inputTestData = Connect (Connect (Vertex "a") (Overlay (Vertex "b") (Vertex "c"))) (Connect (Vertex "d") (Vertex "e"))

-- inputTestData = (Connect (Connect (Overlay (Overlay (Vertex "0") (Overlay (Connect (Overlay (Connect (Overlay (Vertex "1") (Vertex "2")) (Overlay (Vertex "2") (Vertex "3"))) (Overlay (Vertex "2") (Vertex "3"))) (Overlay (Overlay (Connect (Connect (Vertex "2") (Vertex "3")) (Overlay (Connect (Vertex "3") (Vertex "4")) (Vertex "4"))) (Vertex "3")) (Connect (Vertex "3") (Vertex "4")))) (Overlay (Connect (Connect (Vertex "2") (Overlay (Connect (Vertex "3") (Vertex "4")) (Connect (Vertex "4") (Vertex "5")))) (Overlay (Connect (Connect (Overlay (Overlay (Vertex "3") (Vertex "4")) (Vertex "4")) (Connect (Vertex "4") (Overlay (Vertex "5") (Vertex "6")))) (Vertex "4")) (Connect (Connect (Vertex "4") (Vertex "5")) (Connect (Vertex "5") (Overlay (Vertex "6") (Overlay (Overlay (Vertex "7") (Vertex "8")) (Vertex "8"))))))) (Overlay (Connect (Connect (Vertex "3") (Overlay (Vertex "4") (Vertex "5"))) (Vertex "4")) (Vertex "4"))))) (Vertex "1")) (Connect (Vertex "1") (Vertex "2"))) (Vertex "1"))

-- inputTestData = (Connect (Overlay (Connect (Connect (Connect (Vertex "0") (Overlay (Vertex "1") (Connect (Vertex "2") (Connect (Overlay (Overlay (Overlay (Connect (Overlay (Vertex "3") (Overlay (Vertex "4") (Overlay (Vertex "5") (Connect (Connect (Overlay (Vertex "6") (Connect (Vertex "7") (Vertex "8"))) (Connect (Overlay (Vertex "9") (Vertex "10")) (Overlay (Connect (Overlay (Overlay (Vertex "11") (Connect (Connect (Vertex "12") (Connect (Overlay (Vertex "13") (Connect (Vertex "14") (Connect (Vertex "15") (Vertex "16")))) (Vertex "17"))) (Vertex "18"))) (Vertex "19")) (Overlay (Vertex "20") (Vertex "21"))) (Vertex "22")))) (Connect (Connect (Connect (Vertex "23") (Connect (Vertex "24") (Overlay (Vertex "25") (Vertex "26")))) (Overlay (Vertex "27") (Vertex "28"))) (Vertex "29")))))) (Vertex "30")) (Overlay (Vertex "31") (Overlay (Overlay (Vertex "32") (Vertex "33")) (Connect (Overlay (Connect (Vertex "34") (Overlay (Connect (Connect (Vertex "35") (Overlay (Vertex "36") (Vertex "37"))) (Connect (Overlay (Overlay (Vertex "38") (Vertex "39")) (Vertex "40")) (Overlay (Vertex "41") (Overlay (Overlay (Vertex "42") (Vertex "43")) (Vertex "44"))))) (Vertex "45"))) (Overlay (Connect (Vertex "46") (Overlay (Overlay (Connect (Connect (Vertex "47") (Vertex "48")) (Vertex "49")) (Overlay (Vertex "50") (Connect (Vertex "51") (Vertex "52")))) (Vertex "53"))) (Overlay (Vertex "54") (Connect (Overlay (Vertex "55") (Overlay (Overlay (Vertex "56") (Vertex "57")) (Vertex "58"))) (Overlay (Overlay (Overlay (Vertex "59") (Vertex "60")) (Vertex "61")) (Vertex "62")))))) (Vertex "63"))))) (Vertex "64")) (Connect (Overlay (Vertex "65") (Vertex "66")) (Vertex "67"))) (Overlay (Overlay (Connect (Connect (Vertex "68") (Overlay (Overlay (Vertex "69") (Overlay (Vertex "70") (Vertex "71"))) (Connect (Overlay (Vertex "72") (Overlay (Connect (Overlay (Vertex "73") (Vertex "74")) (Vertex "75")) (Overlay (Vertex "76") (Vertex "77")))) (Overlay (Overlay (Vertex "78") (Vertex "79")) (Vertex "80"))))) (Vertex "81")) (Connect (Vertex "82") (Overlay (Vertex "83") (Connect (Vertex "84") (Overlay (Vertex "85") (Overlay (Vertex "86") (Vertex "87"))))))) (Vertex "88")))))) (Vertex "89")) (Connect (Connect (Vertex "90") (Overlay (Vertex "91") (Overlay (Overlay (Overlay (Connect (Connect (Vertex "92") (Vertex "93")) (Vertex "94")) (Connect (Overlay (Connect (Vertex "95") (Overlay (Connect (Connect (Connect (Connect (Overlay (Connect (Connect (Vertex "96") (Vertex "97")) (Connect (Vertex "98") (Overlay (Overlay (Vertex "99") (Overlay (Vertex "100") (Vertex "101"))) (Connect (Vertex "102") (Overlay (Connect (Connect (Overlay (Vertex "103") (Vertex "104")) (Connect (Connect (Vertex "105") (Connect (Vertex "106") (Vertex "107"))) (Connect (Vertex "108") (Vertex "109")))) (Overlay (Vertex "110") (Connect (Connect (Overlay (Vertex "111") (Connect (Overlay (Overlay (Connect (Vertex "112") (Vertex "113")) (Vertex "114")) (Connect (Vertex "115") (Vertex "116"))) (Vertex "117"))) (Overlay (Vertex "118") (Vertex "119"))) (Connect (Vertex "120") (Overlay (Connect (Connect (Overlay (Overlay (Connect (Vertex "121") (Connect (Overlay (Vertex "122") (Connect (Vertex "123") (Vertex "124"))) (Overlay (Vertex "125") (Vertex "126")))) (Overlay (Vertex "127") (Vertex "128"))) (Vertex "129")) (Connect (Vertex "130") (Connect (Vertex "131") (Vertex "132")))) (Vertex "133")) (Connect (Vertex "134") (Overlay (Connect (Connect (Overlay (Overlay (Overlay (Vertex "135") (Vertex "136")) (Vertex "137")) (Vertex "138")) (Vertex "139")) (Overlay (Vertex "140") (Connect (Overlay (Connect (Vertex "141") (Vertex "142")) (Vertex "143")) (Connect (Vertex "144") (Vertex "145"))))) (Vertex "146")))))))) (Vertex "147")))))) (Connect (Vertex "148") (Vertex "149"))) (Overlay (Vertex "150") (Connect (Vertex "151") (Vertex "152")))) (Overlay (Overlay (Vertex "153") (Overlay (Vertex "154") (Vertex "155"))) (Vertex "156"))) (Vertex "157")) (Overlay (Overlay (Connect (Overlay (Vertex "158") (Overlay (Vertex "159") (Connect (Overlay (Overlay (Vertex "160") (Overlay (Vertex "161") (Connect (Vertex "162") (Overlay (Vertex "163") (Overlay (Vertex "164") (Overlay (Vertex "165") (Vertex "166"))))))) (Vertex "167")) (Overlay (Vertex "168") (Connect (Vertex "169") (Connect (Vertex "170") (Connect (Overlay (Connect (Overlay (Connect (Vertex "171") (Connect (Overlay (Overlay (Connect (Vertex "172") (Connect (Vertex "173") (Connect (Vertex "174") (Vertex "175")))) (Vertex "176")) (Overlay (Connect (Overlay (Overlay (Connect (Overlay (Vertex "177") (Vertex "178")) (Overlay (Overlay (Connect (Connect (Overlay (Vertex "179") (Vertex "180")) (Vertex "181")) (Vertex "182")) (Connect (Vertex "183") (Connect (Connect (Vertex "184") (Vertex "185")) (Vertex "186")))) (Vertex "187"))) (Vertex "188")) (Vertex "189")) (Vertex "190")) (Connect (Overlay (Vertex "191") (Connect (Connect (Overlay (Vertex "192") (Overlay (Connect (Vertex "193") (Vertex "194")) (Vertex "195"))) (Vertex "196")) (Connect (Connect (Overlay (Vertex "197") (Overlay (Connect (Vertex "198") (Overlay (Vertex "199") (Vertex "200"))) (Vertex "201"))) (Vertex "202")) (Vertex "203")))) (Connect (Vertex "204") (Overlay (Vertex "205") (Connect (Vertex "206") (Connect (Overlay (Connect (Connect (Overlay (Vertex "207") (Vertex "208")) (Connect (Vertex "209") (Overlay (Overlay (Connect (Overlay (Vertex "210") (Vertex "211")) (Connect (Connect (Vertex "212") (Overlay (Vertex "213") (Vertex "214"))) (Overlay (Vertex "215") (Vertex "216")))) (Overlay (Vertex "217") (Connect (Overlay (Vertex "218") (Overlay (Connect (Connect (Connect (Overlay (Vertex "219") (Connect (Vertex "220") (Vertex "221"))) (Vertex "222")) (Vertex "223")) (Vertex "224")) (Overlay (Vertex "225") (Vertex "226")))) (Overlay (Connect (Vertex "227") (Overlay (Vertex "228") (Vertex "229"))) (Connect (Vertex "230") (Overlay (Vertex "231") (Connect (Overlay (Overlay (Vertex "232") (Overlay (Vertex "233") (Overlay (Vertex "234") (Vertex "235")))) (Vertex "236")) (Vertex "237")))))))) (Vertex "238")))) (Vertex "239")) (Vertex "240")) (Vertex "241")))))))) (Vertex "242"))) (Vertex "243")) (Vertex "244")) (Vertex "245")) (Vertex "246")))))))) (Vertex "247")) (Vertex "248")) (Vertex "249"))) (Vertex "250"))) (Vertex "251")) (Vertex "252"))) (Vertex "253")) (Vertex "254")))) (Vertex "255"))) (Vertex "256")) (Vertex "257"))

-- inputTestData = (Connect (Connect (Vertex "1") (Connect (Connect (Overlay (Connect (Connect (Vertex "2") (Connect (Overlay (Vertex "3") (Vertex "4")) (Overlay (Connect (Vertex "5") (Vertex "6")) (Connect (Overlay (Vertex "7") (Vertex "8")) (Overlay (Vertex "9") (Connect (Vertex "10") (Connect (Vertex "11") (Vertex "12")))))))) (Overlay (Vertex "13") (Vertex "14"))) (Vertex "15")) (Vertex "16")) (Connect (Overlay (Vertex "17") (Overlay (Overlay (Vertex "18") (Vertex "19")) (Vertex "20"))) (Connect (Connect (Vertex "21") (Vertex "22")) (Vertex "23"))))) (Vertex "24"))

-- inputTestData = "(Overlay (Connect (Overlay (Vertex \"1\") (Vertex \"2\")) (Vertex \"3\")) (Connect (Overlay (Connect (Connect (Vertex \"4\") (Vertex \"5\")) (Overlay (Connect (Overlay (Connect (Vertex \"6\") (Vertex \"7\")) (Connect (Overlay (Vertex \"8\") (Vertex \"9\")) (Connect (Overlay (Overlay (Vertex \"10\") (Vertex \"11\")) (Vertex \"12\")) (Overlay (Vertex \"13\") (Connect (Vertex \"14\") (Vertex \"15\")))))) (Vertex \"16\")) (Connect (Vertex \"17\") (Connect (Overlay (Connect (Connect (Connect (Overlay (Vertex \"18\") (Overlay (Connect (Vertex \"19\") (Vertex \"20\")) (Vertex \"21\"))) (Overlay (Vertex \"22\") (Vertex \"23\"))) (Connect (Vertex \"24\") (Vertex \"25\"))) (Vertex \"26\")) (Overlay (Vertex \"27\") (Vertex \"28\"))) (Connect (Vertex \"29\") (Connect (Overlay (Connect (Vertex \"30\") (Overlay (Connect (Vertex \"31\") (Connect (Vertex \"32\") (Overlay (Vertex \"33\") (Overlay (Vertex \"34\") (Connect (Connect (Vertex \"35\") (Connect (Overlay (Connect (Connect (Connect (Vertex \"36\") (Overlay (Vertex \"37\") (Connect (Overlay (Vertex \"38\") (Vertex \"39\")) (Connect (Vertex \"40\") (Connect (Vertex \"41\") (Vertex \"42\")))))) (Vertex \"43\")) (Connect (Overlay (Overlay (Vertex \"44\") (Vertex \"45\")) (Connect (Connect (Overlay (Connect (Connect (Vertex \"46\") (Vertex \"47\")) (Overlay (Overlay (Connect (Vertex \"48\") (Vertex \"49\")) (Vertex \"50\")) (Overlay (Connect (Vertex \"51\") (Vertex \"52\")) (Overlay (Vertex \"53\") (Overlay (Connect (Vertex \"54\") (Overlay (Vertex \"55\") (Connect (Vertex \"56\") (Vertex \"57\")))) (Vertex \"58\")))))) (Connect (Overlay (Overlay (Vertex \"59\") (Vertex \"60\")) (Vertex \"61\")) (Connect (Vertex \"62\") (Vertex \"63\")))) (Overlay (Vertex \"64\") (Connect (Overlay (Vertex \"65\") (Connect (Vertex \"66\") (Overlay (Overlay (Vertex \"67\") (Connect (Vertex \"68\") (Vertex \"69\"))) (Vertex \"70\")))) (Vertex \"71\")))) (Connect (Vertex \"72\") (Overlay (Connect (Connect (Connect (Overlay (Vertex \"73\") (Vertex \"74\")) (Vertex \"75\")) (Connect (Vertex \"76\") (Overlay (Vertex \"77\") (Connect (Vertex \"78\") (Vertex \"79\"))))) (Vertex \"80\")) (Vertex \"81\"))))) (Vertex \"82\"))) (Overlay (Overlay (Connect (Overlay (Vertex \"83\") (Connect (Overlay (Overlay (Vertex \"84\") (Connect (Connect (Vertex \"85\") (Connect (Vertex \"86\") (Overlay (Overlay (Connect (Vertex \"87\") (Vertex \"88\")) (Vertex \"89\")) (Vertex \"90\")))) (Overlay (Overlay (Overlay (Overlay (Vertex \"91\") (Vertex \"92\")) (Vertex \"93\")) (Connect (Vertex \"94\") (Vertex \"95\"))) (Vertex \"96\")))) (Vertex \"97\")) (Vertex \"98\"))) (Vertex \"99\")) (Vertex \"100\")) (Vertex \"101\"))) (Vertex \"102\"))) (Vertex \"103\")))))) (Vertex \"104\"))) (Vertex \"105\")) (Vertex \"106\"))))))) (Vertex \"107\")) (Vertex \"108\")))"
inputTestData = (Overlay (Connect (Connect (Connect (Vertex 1) (Connect (Vertex 2) (Vertex 3))) (Vertex 4)) (Overlay (Overlay (Overlay (Vertex 5) (Vertex 6)) (Connect (Connect (Vertex 7) (Connect (Overlay (Connect (Overlay (Connect (Vertex 8) (Connect (Vertex 9) (Vertex 10))) (Vertex 11)) (Vertex 12)) (Vertex 13)) (Vertex 14))) (Vertex 15))) (Overlay (Vertex 16) (Connect (Overlay (Connect (Vertex 17) (Connect (Overlay (Vertex 18) (Vertex 19)) (Vertex 20))) (Vertex 21)) (Overlay (Overlay (Overlay (Vertex 22) (Vertex 23)) (Connect (Connect (Vertex 24) (Vertex 25)) (Vertex 26))) (Vertex 27)))))) (Vertex 28))



