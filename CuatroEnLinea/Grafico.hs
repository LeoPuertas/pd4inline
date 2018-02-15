{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module CuatroEnLinea.Grafico where

import CuatroEnLinea.Tipos 
import CuatroEnLinea.Piezas

import Data.Matrix as M
import Graphics.Gloss as G


type Point = (Float, Float)

type Vector = G.Point

type Path = [G.Point]


data Picture = Line    G.Path
             | Polygon G.Path
             | Text    String


mostrarTablero :: Params -> IO()
mostrarTablero mat = display (InWindow "Cuatro en Linea" (800,750) (100,300)) (dark yellow) (tableroIO mat)



tableroIO :: Params-> G.Picture
tableroIO mat@Params{matriz = m} = 
    G.Pictures [board,fichasX,fichasO,fichasV]
    where board = tablero mat
          fichasX = getFichasX m
          fichasO = getFichasO m
          fichasV = getFichasVacias m



getFichasX :: Matrix Char-> G.Picture
getFichasX  m = G.Pictures  [ fichaX (toFloat y) (toFloat x) | y <-[1,2..7], x <- [2,3..8], getElem y x m == 'X']

getFichasO :: Matrix Char -> G.Picture
getFichasO  m = G.Pictures  [ fichaO (toFloat y) (toFloat x) | y <-[1,2..7], x <- [2,3..8], getElem y x m == 'O']

getFichasVacias :: Matrix Char -> G.Picture
getFichasVacias  m =  fichasVacias (show(length [ x | y <-[1,2..7], x <- [2,3..8], getElem y x m == ' ']))



toFloat:: Int-> Float
toFloat x = fromIntegral x::Float



tablero :: Params -> G.Picture
tablero para = G.Pictures [titulo para,numero1,numeros,lineasHorizontal,lineasVertical]


lineasHorizontal::G.Picture
lineasHorizontal= G.Pictures [G.Line[(-319, x),(220,x)] | x<-[178,118..(-182)]]

lineasVertical::G.Picture
lineasVertical= G.Pictures [G.Line[(x, 178),(x,-182)] | x<-[(-319),(-242)..220]]


numero1:: G.Picture
numero1 = Translate (-305) (-225) $ 
         Scale 1 0.3 $ 
         G.Text "1"

numeros::G.Picture
numeros  = Translate (-398) (-225) $ 
         Scale 1 0.3 $ 
         G.Text "* 234567*" 


titulo:: Params -> G.Picture
titulo para = Translate (-280) 248 $ 
         Scale 0.4 0.3 $ 
         G.Text ("GANO EL JUGADOR " ++ [getUltPieza para])


fichaX :: Float -> Float  -> G.Picture
fichaX y x = Translate (-430 + (77 * x)) ( 270 - (60 * y)) $ 
                Scale 0.2 0.2 $ 
                Color red $
                circleSolid 130


fichaO :: Float -> Float  -> G.Picture
fichaO y x = Translate (-430 + (77 * x)) ( 270 - (60 * y)) $ 
                Scale 0.2 0.2 $ 
                Color blue $
                circleSolid 130

fichasVacias::String->G.Picture
fichasVacias x  = Translate (-378) 210 $ 
                Scale 0.2 0.2 $ 
                G.Text ("Cantidad de tiros restantes :  "++ x)


