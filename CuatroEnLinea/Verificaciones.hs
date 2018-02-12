

module CuatroEnLinea.Verificaciones where

import CuatroEnLinea.Constantes
import CuatroEnLinea.Funciones
import CuatroEnLinea.Piezas
import CuatroEnLinea.Tipos
import CuatroEnLinea.Matriz

import Data.Matrix

vVictoria :: Params -> Bool
vVictoria params = (vVertical || vHorizontal || vDiagonalI || vDiagonalD)
        where vHorizontal = vVictoriaH  params 
              vVertical   = vVictoriaV    params 
              vDiagonalI  = vVictoriaDiagI   params 
              vDiagonalD  = vVictoriaDiagD   params 

vVictoriaH :: Params -> Bool 
vVictoriaH w@(Params{ matriz = m, ultimoTiro = (row , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where
       x = getSubStringInit (cuatroPiezas w) $getFila m (row-1)

vVictoriaV :: Params -> Bool
vVictoriaV w@(Params{ matriz = m, ultimoTiro = (row , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where 
       x = getSubStringInit (cuatroPiezas w) $getColumna m (col-1)


vVictoriaDiagD :: Params -> Bool
vVictoriaDiagD params 
     | d /= Nothing = True
     | otherwise = False
     where d = getSubStringInit (cuatroPiezas params) (diagonalD params)

vVictoriaDiagI :: Params -> Bool
vVictoriaDiagI params
     | d /= Nothing =True
     | otherwise = False
     where d =  getSubStringInit (cuatroPiezas params) (diagonalI params)


vBloquoVertical:: Params -> Bool
vBloquoVertical w@(Params{ matriz = m, ultimoTiro = (row , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where 
       x = getSubStringInit (bloqueoVertical)  $getColumna m (col-1)


lleno :: Params -> Bool
lleno w@(Params{matriz = m}) = notElem vacio ((toList $transpose m)) 

vColumnaLlena :: Params -> Int -> Bool
vColumnaLlena w@(Params{matriz = m}) col = elem vacio ((toLists $transpose m)!!(col))
     