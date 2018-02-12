

module CuatroEnLinea.Matriz where

import CuatroEnLinea.Tipos
import CuatroEnLinea.Constantes
import CuatroEnLinea.Piezas

import Data.Matrix

diagonalD:: Params -> [Char]
diagonalD w@(Params {matriz = m, ultimoTiro = (y,x)}) = reverse[(getElem (y+p) (x-p) m) | p <- [-8..8], (x-p) > 0, (y+p) > 0, (x-p) < 10, (y+p) < 9]

diagonalI:: Params -> [Char]
diagonalI w@(Params {matriz = m, ultimoTiro = (y,x)})  = reverse[(getElem (y-cx) (x-cx) m) | cx<-[-8..8], (x-cx) > 0, (y-cx) > 0, (x-cx) < 9, (y-cx) < 9]
    
getFila :: Matrix Char -> Int -> [Char]
getFila mat row = (toLists mat)!!row

getColumna :: Matrix Char -> Int -> [Char]
getColumna mat col = (toLists $transpose mat)!!col


vertical :: Params -> Int
vertical w@(Params{ matriz = mat, ultimoTiro = (row , col),ultimoTiroPc = (rowPC,colPC),nroTiro = n}) 
      | getUltPieza w == getElem (row + 1) col mat = 1 + vertical' mat (row) (col) (getUltPieza w)
      | otherwise = 0

vertical2 :: Params -> Int
vertical2 w@(Params{ matriz = mat, ultimoTiro = (row , col),ultimoTiroPc = (rowPC,colPC),nroTiro = n}) 
      | getSigPieza w == getElem (rowPC +1) colPC mat = 1 + vertical' mat (rowPC) (colPC) (getSigPieza w)
      | otherwise = 0

vertical' :: Matrix Char -> Int -> Int->Char ->Int
vertical' mat row  col pieza
      | pieza == getElem (row+1) col mat = 1 + vertical' mat (row+1) col pieza
      | otherwise = 0


horizontal :: Params -> [Char] 
horizontal w@(Params{ matriz = m,ultimoTiro = (row , col),nroTiro = n}) = getFila m (row-1)


horizontal2 :: Params -> [Char] 
horizontal2 w@(Params{ matriz = m,ultimoTiroPc = (rowPC,colPC),nroTiro = n}) = getFila m (rowPC-1)

