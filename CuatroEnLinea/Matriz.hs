

module CuatroEnLinea.Matriz where

import CuatroEnLinea.Tipos
import CuatroEnLinea.Piezas

import Data.Matrix

--FUNCION QUE DEVUELVE EL VECTOR DEL ULTIMO TIRO FORMADO POR UNA DIAGONAL CON LA PARTE MAYOR A LA DERECHA DEL TIRO DE REFERENCIA

diagonalD:: Params -> String
diagonalD Params {matriz = m, ultimoTiro = (y,x)} = reverse[getElem (y+p) (x-p) m | p <- [-8..8], (x-p) > 0, (y+p) > 0, (x-p) < 10, (y+p) < 9]

--FUNCION QUE DEVUELVE EL VECTOR DEL ULTIMO TIRO FORMADO POR UNA DIAGONAL CON LA PARTE MAYOR A LA IZQUIERDA DEL TIRO DE REFERENCIA
diagonalI:: Params -> String
diagonalI Params {matriz = m, ultimoTiro = (y,x)}  = reverse[getElem (y-cx) (x-cx) m | cx<-[-8..8], (x-cx) > 0, (y-cx) > 0, (x-cx) < 9, (y-cx) < 9]
    
getFila :: Matrix Char -> Int -> String
getFila mat row = toLists mat !! row

getColumna :: Matrix Char -> Int -> String
getColumna mat col = (toLists $transpose mat)!!col

--FUNCION QUE DEVUELVE LA COLUMNA SEGUN EL ULTIMO TIRO DEL JUGADOR
verticalTiroJugador :: Params -> Int
verticalTiroJugador w@Params{ matriz = mat, ultimoTiro = (row , col)}
      | pieza == getElem (row + 1) col mat = 1 + vertical' mat row col pieza
      | otherwise = 0
      where 
        pieza = getUltPieza w 

--FUNCION QUE DEVUELVE LA COLUMNA SEGUN EL ULTIMO TIRO DEL PC
verticalTiroPC :: Params -> Int
verticalTiroPC w@Params{ matriz = mat,ultimoTiroPc = (rowPC,colPC)} 
      | pieza == getElem (rowPC +1) colPC mat = 1 + vertical' mat rowPC colPC pieza
      | otherwise = 0
      where pieza = getSigPieza w

--FUNCION QUE DEVULVE CUANTOS ELEMENTOS IGUALES CONSECUTIVOS HAY DEBAJO DEL QUE SE TIRO
vertical' :: Matrix Char -> Int -> Int-> Char ->Int
vertical' mat row  col pieza
      | pieza == getElem (row+1) col mat = 1 + vertical' mat (row+1) col pieza
      | otherwise = 0

--FUNCION QUE DEVUELVE LA FILA SEGUN EL ULTIMO TIRO DEL JUGADOR
horizontal :: Params -> String 
horizontal Params{ matriz = m,ultimoTiro = (row , _)} = getFila m (row-1)

--FUNCION QUE DEVUELVE LA FILA SEGUN EL ULTIMO TIRO DEL PC
horizontal2 :: Params -> String
horizontal2 Params{ matriz = m,ultimoTiroPc = (rowPC,_)} = getFila m (rowPC-1)

horizontal' ::Params -> Int -> String
horizontal' Params{ matriz = m} row = getFila m (row-1)

