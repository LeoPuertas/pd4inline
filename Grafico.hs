{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


import Data.Matrix as M
import Control.Monad
import Data.List as L
import Text.Read
import System.Console.ANSI -- cabal install ansi-terminal     
import Graphics.Gloss as G
import Data.Monoid




matriz :: Matrix Char
matriz = fromLists ["*********","*O      *","*X      *","*X      *","*O X    *","*XXOX   O*","*XOXXOOO*","*1234567*"]



type Point = (Float, Float)

type Vector = G.Point

type Path = [G.Point]


data Picture = Line    G.Path
             | Polygon G.Path
             | Text    String






main = tableroConFichas






tableroVacio :: IO()
tableroVacio = display (InWindow "4 IN LINE" (800, 750) (100, 300)) (dark yellow) (tablero)


--tableroConFichas :: IO()
--tableroConFichas = display (InWindow "4 IN LINE" (800, 750) (100, 300)) (dark yellow) (fichas)

tableroConFichas :: IO()
tableroConFichas = display(InWindow "4 IN LINE" (800, 750) (100, 300)) (dark yellow) (fichas)



toFloat:: Int-> Float
toFloat x = (fromIntegral(x))::Float


fichasXenMatriz :: G.Picture
fichasXenMatriz= G.Pictures  [ fichaX (toFloat y) (toFloat x) | y <-[1,2..7], x <- [2,3..8], (getElem y x matriz) == 'X']

fichasOenMatriz :: G.Picture
fichasOenMatriz= G.Pictures  [ fichaO (toFloat y) (toFloat x) | y <-[1,2..7], x <- [2,3..8], (getElem y x matriz) == 'O']

fichasVaciasenMatriz :: G.Picture
fichasVaciasenMatriz=  fichasVacias (show(length [ x | y <-[1,2..7], x <- [2,3..8], (getElem y x matriz) == ' ']))



--mat :: G.Picture
--mat = G.Pictures [fila1,fila2,fila3]


--fila1 :: Int-> G.Picture
--fila1 = G.Pictures [cuadrado1 ,cuadrado2,cuadrado3,cuadrado4 ,cuadrado5,cuadrado6,cuadrado7 ,cuadrado8,cuadrado9]



tablero :: G.Picture
tablero = G.Pictures [titulo,linea2,linea3,linea4,linea5,linea6,linea7,linea8,linea10,linea11,linea12,linea13,linea14,linea15,linea16,linea17,linea19,linea20]


fichas :: G.Picture
fichas = G.Pictures [tablero,fichasXenMatriz,fichasOenMatriz,fichasVaciasenMatriz]



linea2:: G.Picture
linea2 = G.Line[(-319,178),(220,178)]

linea3:: G.Picture
linea3 = G.Line[(-319,118),(220,118)]

linea4:: G.Picture
linea4 = G.Line[(-319,58),(220,58)]

linea5:: G.Picture
linea5 = G.Line[(-319,-2),(220,-2)]

linea6:: G.Picture
linea6 = G.Line[(-319,-62),(220,-62)]

linea7:: G.Picture
linea7 = G.Line[(-319,-122),(220,-122)]

linea8:: G.Picture
linea8 = G.Line[(-319,-182),(220,-182)]


linea10:: G.Picture
linea10 = G.Line[(-319,178),(-319,-182)]

linea11:: G.Picture
linea11 = G.Line[(-242,178),(-242,-182)]

linea12:: G.Picture
linea12 = G.Line[(-165,178),(-165,-182)]

linea13:: G.Picture
linea13 = G.Line[(-88,178),(-88,-182)]

linea14:: G.Picture
linea14 = G.Line[(-11,178),(-11,-182)]

linea15:: G.Picture
linea15 = G.Line[(66,178),(66,-182)]

linea16:: G.Picture
linea16 = G.Line[(143,178),(143,-182)]

linea17:: G.Picture
linea17 = G.Line[(220,178),(220,-182)]


linea21:: G.Picture
linea21 = G.Line[(-270,238),(-270,-182)]

linea19:: G.Picture
linea19 = Translate (-305) (-225) $ 
         Scale 1 0.3 $ 
         G.Text "1"

linea20::G.Picture
linea20  = Translate (-398) (-225) $ 
         Scale 1 0.3 $ 
         G.Text "* 234567*" 



titulo:: G.Picture
titulo  = Translate (-378) (248) $ 
         Scale 0.2 0.2 $ 
         G.Text "CLICK SOBRE EL NUMERO DE LA COLUMNA" 




fichaX :: Float -> Float  -> G.Picture
fichaX y x = Translate (-430 + (77 * (x))) ( 270 - (60 * y)) $ 
                Scale 0.2 0.2 $ 
                Color red $
                circleSolid 130




fichaO :: Float -> Float  -> G.Picture
fichaO y x = Translate (-430 + (77 * (x))) ( 270 - (60 * y)) $ 
                Scale 0.2 0.2 $ 
                Color blue $
                circleSolid 130

fichasVacias::String->G.Picture
fichasVacias x  = Translate (-378) (210) $ 
                Scale 0.2 0.2 $ 
                G.Text ("Cantidad de tiros restantes :  "++ x)
