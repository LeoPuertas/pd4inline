

module CuatroEnLinea.Verificaciones where

import CuatroEnLinea.Constantes
import CuatroEnLinea.Funciones
import CuatroEnLinea.Piezas
import CuatroEnLinea.Tipos
import CuatroEnLinea.Matriz

import Data.Matrix

--FUNCION QUE DEVULVE SI UN JUGAOR CUMPLIO ALGUNA DE LAS 4 POSIBILIDADES PARA GANAR
vVictoria :: Params -> Bool
vVictoria params = (vVertical || vHorizontal || vDiagonalI || vDiagonalD)
        where vHorizontal = vVictoriaH  params 
              vVertical   = vVictoriaV    params 
              vDiagonalI  = vVictoriaDiagI   params 
              vDiagonalD  = vVictoriaDiagD   params 

--VERIFICA SI HORIZONTALMENTE HAY CUATRO FICHAS CONSECUTIVAS
vVictoriaH :: Params -> Bool 
vVictoriaH w@(Params{ matriz = m, ultimoTiro = (row , _)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where
       x = getSubStringInit (cuatroPiezas w) $getFila m (row-1)

--VERIFICA SI VERTICALMENTE HAY CUATRO FICHAS CONSECUTIVAS
vVictoriaV :: Params -> Bool
vVictoriaV w@(Params{ matriz = m, ultimoTiro = (_ , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where 
       x = getSubStringInit (cuatroPiezas w) $getColumna m (col-1)

--VERIFICA SI DIAGONALMENTE CRECIENTE A LA DERECHA HAY CUATRO FICHAS CONSECUTIVAS
vVictoriaDiagD :: Params -> Bool
vVictoriaDiagD params 
     | d /= Nothing = True
     | otherwise = False
     where d = getSubStringInit (cuatroPiezas params) (diagonalD params)

--VERIFICA SI DIAGONALMENTE CRECIENTE A LA IZQUIERDA HAY CUATRO FICHAS CONSECUTIVAS
vVictoriaDiagI :: Params -> Bool
vVictoriaDiagI params
     | d /= Nothing = True
     | otherwise = False
     where 
      d =  getSubStringInit (cuatroPiezas params) (diagonalI params)

--FUNCION QUE INDICA SI HUBO UNA JUGADA DONDE HABIA 3 FICHAS CONSECUTIVAS PERO SE BLOQUEO ESA JUGADA, SE UTILIZA PARA NO SEGUIR TIRANDO EN ESA COLUMNA 
--INTENTANDO FORMAR UNA VICTORIA VERTICAL YA QUE NO ES POSIBLE
vBloquoVertical:: Params -> Bool
vBloquoVertical (Params{ matriz = m, ultimoTiro = (_ , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where 
       x = getSubStringInit (bloqueoVertical)  $getColumna m (col-1)

--FUNCION QUE DEVUELVE SI LA MATRIZ ESTA LLENA PARA SABER SI FUE UN EMPATE
lleno :: Params -> Bool
lleno (Params {nroTiro = n})
    | n == 42 = True   
    | otherwise = False 

--FUNCION QUE DEVUELVE SI LA COLUMNA ESTA LLENA PARA INDICAR QUE PERDIO EL TIRO O QUE LA PC NO TIRE EN DICHA COLUMNA
vColumnaLlena :: Params -> Int -> Bool
vColumnaLlena (Params{matriz = m}) col = elem vacio ((toLists $transpose m)!!(col))
     