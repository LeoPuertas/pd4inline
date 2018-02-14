{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module CuatroEnLinea.Grafico where

import CuatroEnLinea.Tipos 
import CuatroEnLinea.Piezas

import Data.Matrix as M
import Control.Monad
import Data.List as L
import System.Console.ANSI -- cabal install ansi-terminal     
import Graphics.Gloss as G
import Data.Monoid
import Data.Monoid ((<>))



type Point = (Float, Float)

type Vector = G.Point

type Path = [G.Point]


data Picture = Line    G.Path
             | Polygon G.Path
             | Text    String


mostrarTablero :: Params -> IO()
mostrarTablero mat@(Params{matriz = m})= display (InWindow "Cuatro en Linea" (800,750) (100,300)) (dark yellow) (tableroIO mat)



tableroIO :: Params-> G.Picture
tableroIO mat@(Params{matriz = m}) = 
    G.Pictures [board,fichasX,fichasO,fichasV]
    where board = tablero mat
          fichasX = getFichasX m
          fichasO = getFichasO m
          fichasV = getFichasVacias m



getFichasX :: Matrix Char-> G.Picture
getFichasX  m = G.Pictures  [ fichaX (toFloat y) (toFloat x) | y <-[1,2..7], x <- [2,3..8], (getElem y x m) == 'X']

getFichasO :: Matrix Char -> G.Picture
getFichasO  m = G.Pictures  [ fichaO (toFloat y) (toFloat x) | y <-[1,2..7], x <- [2,3..8], (getElem y x m) == 'O']

getFichasVacias :: Matrix Char -> G.Picture
getFichasVacias  m =  fichasVacias (show(length [ x | y <-[1,2..7], x <- [2,3..8], (getElem y x m) == ' ']))



toFloat:: Int-> Float
toFloat x = (fromIntegral(x))::Float



tablero :: Params -> G.Picture
tablero para = G.Pictures [titulo para,linea2,linea3,linea4,linea5,linea6,linea7,linea8,linea10,linea11,linea12,linea13,linea14,linea15,linea16,linea17,linea19,linea20]



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


titulo:: Params -> G.Picture
titulo para = Translate (-280) (248) $ 
         Scale 0.4 0.3 $ 
         G.Text ("GANO EL JUGADOR " ++ [getUltPieza para])


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
