
module CuatroEnLinea.Jugadas where

import CuatroEnLinea.Tipos
import CuatroEnLinea.Constantes
import CuatroEnLinea.Piezas
import CuatroEnLinea.Funciones
import CuatroEnLinea.Verificaciones
import CuatroEnLinea.Matriz

import Data.Matrix

getFilaTiro :: Params -> (Int , Int) -> Int
getFilaTiro w@(Params {matriz = m}) (row, col)  
    | x /= vacio = getFilaTiro w ((row-1), col)
    | otherwise = row
   where x = getElem row col m

      
tiro' ::Params -> Int -> Params
tiro' w@(Params{ matriz = m, nroTiro = t}) col 
    | (even t) && (vColum) = w {matriz = newMat, nroTiro = t + 1, ultimoTiro = ultTiro, perdido = 0}
    | (odd t) &&  (vColum) = w {matriz = newMat, nroTiro = t + 1, ultimoTiro = ultTiro, ultimoTiroPc = ultTiro, perdido = 0}
    | otherwise = perdioTiro w
      where row = getFilaTiro w (8, (col+1))
            pieza = getSigPieza w
            newMat = setElem (pieza) ((row),(col+1)) m
            ultTiro = (row, col+1)
            vColum = vColumnaLlena w (col) 

tiroPC :: Params -> Int
tiroPC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
         | 1 == (victoria  w) = 1
         | 20 < verificacion = verificacion - 20
         | 20 < verificacionPC = verificacionPC - 20
         | length barrer > 20 = head barrer - 21 --resto 1 mas porque el tiro lleva 1 columna menos
         | length barrer > 0 =  head barrer
         | 1 <= verificar' w= verificar'  w 
         | 10 > verificacion  && 0 < verificacion  = verificacion 
         | 10 > verificacionPC  && 0 < verificacionPC  = verificacionPC
         | 1 <= verificacion = verificacion
         | 1 <= verificacionPC = verificacionPC
         | otherwise = row
           where
                 verificacion = (verificar'PC  w)
                 verificacionPC = (verificarUltimoPC w)
                 barrer = barreFilasyColumnas w

perdioTiro :: Params -> Params
perdioTiro w@(Params{perdido = p}) = w { perdido = 1}

--------------------------------------------------------
verificar2 :: String -> Int
verificar2 row 
    | v1 /= Nothing = 20
    | v2 /= Nothing = 10 + (getNum v2) 
    | v3 /= Nothing = 10 + (getNum v3)       
    | v4 /= Nothing = 10 + (getNum v4) + 3
    | v5 /= Nothing = 10 + (getNum v5) + 2
    | v6 /= Nothing = 10 + (getNum v6) + 1
    | otherwise = -1
        where 
          v1 = getSubStringInit "XXXX" row
          v2 = getSubStringInit " XX " row
          v3 = getSubStringInit " XXX" row
          v4 = getSubStringInit "XXX " row
          v5 = getSubStringInit "XX X" row
          v6 = getSubStringInit "X XX" row

verificar' :: Params -> Int 
verificar' w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
    | (0 < verifica2H ) && (getElem (row + 1) (verifica2H-10)  mat /= ' ')  =  verifica2H - 10 --Si la ficha de abajo existe la coloca sino evita
    | (0 <  verifica2D) = verifica2D - 10
    | (0 <  verifica2I)= verifica2I - 10
    | (vertic == 3) = col-1
    | otherwise = -1
        where 
             verifica2H = verificar2 (horizontal  w)
             verifica2D = verificar2 (diagonalD  w)
             verifica2I = verificar2 (diagonalI  w)
             vertic = vertical w

victoria :: Params -> Int
victoria w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
    | 20 == (verificar2 (horizontal w)) = 1 
    | 20 == (verificar2 (diagonalD w)) = 1
    | 20 == (verificar2 (diagonalI w)) = 1
    | 4 ==  vertical w = 1
    | otherwise = 0


fichaVertical :: Params -> Int
fichaVertical w@(Params{ matriz = mat, ultimoTiro = (row , col)}) = sum[ 1 | x <- [7,6..1], let elem = getElem (x) col mat, elem /= ' ',elem /= '*']


------------------------------JUGADA AUTOMATICA POR PC

barreFilasyColumnas:: Params -> [Int] 
barreFilasyColumnas w@(Params{ matriz = mat})= [ vFyC | row <- [7,6..1], col <- [1,3..7], let vFyC = (verificaFilayColumna mat row col), vFyC > 0 ]



verificarUltimoPC :: Params -> Int 
verificarUltimoPC w@(Params{ matriz = mat, ultimoTiroPc = (row , col)}) 
  | (20 < verificaH) && (getElem (row + 1) col mat /= ' ')  =  verificaH -21
  | (0 < verificaH )  && (20 > verificaH )= verificaH
  | (0 < verificaD) && (20 > (verificaD)) = verificaD
  | (0 < verificaI) && (20 > (verificaI))= verificaI
  | (vertical2 w /= 0) &&  ( vertical2 w == 3) && ( not (vBloquoVertical w)) && (vColumnaLlena w (col)) = 19 + col
  | otherwise = -1
        where 
             verificaH = verificarPC (horizontal  w)
             verificaD = verificarPC (diagonalD w)
             verificaI = verificarPC (diagonalI w)


verificarPC :: String -> Int
verificarPC row 
    | v1 /= Nothing = 20
    | v2 /= Nothing = 20 + (getNum v2)       
    | v3 /= Nothing = 20 + (getNum v3) + 3 
    | v4 /= Nothing = 20 + (getNum v4) + 2
    | v5 /= Nothing = 20 + (getNum v5) + 1
    | v6 /= Nothing = (getNum v6) + 1
    | v7 /= Nothing = (getNum v7) 
    | v8 /= Nothing = (getNum v8) + 2
    | otherwise = -1
    where
         v1 = getSubStringInit "OOOO" row 
         v2 = getSubStringInit " OOO" row
         v3 = getSubStringInit "OOO " row
         v4 = getSubStringInit "OO O" row 
         v5 = getSubStringInit "O OO" row
         v6 = getSubStringInit "OO " row 
         v7 = getSubStringInit " OO" row
         v8 = getSubStringInit " O O" row



    --verificaciones para ver
    --getSubStringInit "O O" row /= Nothing = (getNum $getSubStringInit "O O" row) + 2
    --getSubStringInit " O " row /= Nothing = (getNum $getSubStringInit " O " row) + 2
    --getSubStringInit " O" row /= Nothing = (getNum $getSubStringInit " O" row) + 1
    --getSubStringInit "O " row /= Nothing = (getNum $getSubStringInit "O " row) + 2
    


verificar'PC :: Params -> Int 
verificar'PC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
  | (20 < verificaH) && (getElem (row + 1) col mat /= ' ')  =  verificaH -20
  | (0 < verificaH )  && (20 > verificaH )= verificaH
  | (0 < verificaD) && (20 > (verificaD)) = verificaD
  | (0 < verificaI) && (20 > (verificaI))= verificaI
  | (vertical2 w /= 0) &&  (vertical2 w == 3) && ( not (vBloquoVertical w)) && (vColumnaLlena w (col)) = 19 + col
  | otherwise = -1
        where 
             verificaH = verificarPC (horizontal2  w)
             verificaD = verificarPC (diagonalD w)
             verificaI = verificarPC (diagonalI w)


verificaFilayColumna:: Matrix Char -> Int -> Int -> Int
verificaFilayColumna mat row col 
          | v1 > 20  = col
          | v2 > 20  = verificarPC (getFila mat (row)) 
          |otherwise = -1
          where 
            v1 =  verificarPC (getColumna mat (col-1))
            v2 =  verificarPC (getFila mat (row-1))



victoriaPC :: Params  -> Int
victoriaPC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
        | 20 == (verificarPC (horizontal  w)) = 1 
        | 20 == (verificarPC (diagonalD w)) = 1
        | 20 == (verificarPC (diagonalI w)) = 1
        | 4 == 1 + vertical w = 1
        | otherwise = 0
