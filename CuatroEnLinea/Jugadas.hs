
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

 
--FUNCION QUE REALIZA EL TIRO TANTO PARA PC COMO PARA EL JUGADOR, IDENTIFICA SI LA COLUMNA ESTA LLENA LO MARCA COMO PERDIDO      
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

--FUNCION QUE DEVUELVE LA COLUMNA DONDE TIENE QUE TIRAR LA PC CON LAS SIGUIENTES PRIORIDADES
--SI CON EL ULTIMO TIRO LE PERMITE TERMINAR UNA JUGADA PENDIENTE PARA GANAR
--VERIFICA SI EL JUGADOR TIENE 3 CONSECUTIVAS SI PUEDE BLOQUEAR ESA JUGADA
--BARRE LA MATRIZ PARA ENCONTRAR UNA JUGADA PENDIENTE SIN RELACION A LOS ULTIMOS TIROS
--REALIZA EL TIRO POR PROXIMIDAD A OTRA FICHA
--
tiroPC :: Params -> Int 
tiroPC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
         | vicPC /= -1 = vicPC
         | vicUser /= -1 = vicUser
         | jugadaPc /= -1 = jugadaPc
         | length barrerJugada > 0 =  head barrerJugada 
         | otherwise = row
         where 
          vicPC = vVictoriaPC w
          vicUser = vVictoriaUser w
          jugadaPc = vPosibJugadaPc w
          barrerJugada = barreFilasyColumnas w False

-----------------------------------------------------------
vVictoriaPC :: Params -> Int
vVictoriaPC w@(Params{ matriz = mat, ultimoTiroPc = (rowPc , colPc), ultimoTiro = (row , col)})
           | vJuVictoriaHUser /= -1 && (getElem (row + 1) col mat /= vacio) = vJuVictoriaHUser -1
           | vJuVictoriaHPC   /= -1 && (getElem (rowPc + 1) colPc mat /= vacio) = vJuVictoriaHPC
           | (verticalTiroPC w == 3) && (not (vBloquoVertical w)) && (vColumnaLlena w (colPc)) =  colPc-1 
           | vJuVictoriaDD /= -1 = vJuVictoriaDD
           | vJuVictoriaDI /= -1 = vJuVictoriaDI
           | length barrerGanar > 0 =  head barrerGanar  
           | otherwise = -1
           where 
             vJuVictoriaHUser  = vJugadaVictoriaPc (horizontal' w row) --horaizontal ultimo tiro user
             vJuVictoriaHPC    = vJugadaVictoriaPc (horizontal' w rowPc) --horaizontal ultimo tiro PC
             vJuVictoriaDD = vJugadaVictoriaPc (diagonalD w)  
             vJuVictoriaDI = vJugadaVictoriaPc (diagonalI w)
             barrerGanar = barreFilasyColumnas w True
             
vVictoriaUser :: Params -> Int 
vVictoriaUser w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
    | (verificaH /= -1) && (getElem (row + 1) (verificaH)  mat /= vacio)  =  verificaH  --Si la ficha de abajo existe la coloca sino evita
    | (verificaD /= -1) = verificaD 
    | (verificaI /= -1) = verificaI 
    | (vertic == 3) = col-1
    | otherwise = -1
        where 
             verificaH = vBloqVictoriaUser (horizontal  w)
             verificaD = vBloqVictoriaUser (diagonalD  w)
             verificaI = vBloqVictoriaUser (diagonalI  w)
             vertic = verticalTiroJugador w

vPosibJugadaPc :: Params -> Int 
vPosibJugadaPc w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
    | (verificaH /= -1) && (getElem (row + 1) (verificaH)  mat /= vacio)  =  verificaH  --Si la ficha de abajo existe la coloca sino evita
    | (verificaH /= -1) = verificaH
    | (verificaD /= -1) = verificaD 
    | (verificaI /= -1) = verificaI 
    | (vertic == 3) = col-1
    | otherwise = -1
        where 
             verificaH = vPosibleVicoriaPC (horizontal w)
             verificaD = vPosibleVicoriaPC (diagonalD  w)
             verificaI = vPosibleVicoriaPC (diagonalI  w)
             vertic = verticalTiroJugador w

------------------------------------------------------------------------------------
--SI TIRO EN UNA COLUMNA LLENA SE MARCA EL TIRO PERDIDO PARA DAR EL MENSAJE
perdioTiro :: Params -> Params
perdioTiro w@(Params{perdido = p}) = w { perdido = 1}

--FUNCION UTILIZADA EN CASO DE QUE NINGUNO DE LOS DOS ULTIMOS TIROS SIRVA DE REFERENCIAA PARA UNA JUGADA, ESTA FUNCION DEVUELVE SI HAY ALGUNA PENDIENTE SIN REFERENCIA
barreFilasyColumnas:: Params -> Bool -> [Int] 
barreFilasyColumnas w@(Params{ matriz = mat}) ganar = [ vFyC | row <- [7,6..1], col <- [1..8], 
              let vFyC = (if ganar 
                             then (vFilayColumnaGanar mat row col) 
                             else (vFilayColumnaJugada mat row col)), vFyC > 0 ]

--FUNCION QUE SE UTILIZA CON EL BARRER FILAS Y COLUMNAS PARA SABER SI EXISTE UNA JUGADA PENDIENTE SIN REFERENCIA A LOS ULTIMOS TIROS
vFilayColumnaGanar:: Matrix Char -> Int -> Int -> Int
vFilayColumnaGanar mat row col 
          | v1 /= -1 = col -1
          | v2 /= -1 = vJugadaVictoriaPc (getFila mat (row)) 
          |otherwise = -1
          where 
            v1 =  vJugadaVictoriaPc (getColumna mat (col-1))
            v2 =  vJugadaVictoriaPc (getFila mat (row-1))

vFilayColumnaJugada:: Matrix Char -> Int -> Int -> Int
vFilayColumnaJugada mat row col 
          | v1 /= -1 = col -1
          | v2 /= -1 = vPosibleVicoriaPC (getFila mat (row)) 
          |otherwise = -1
          where 
            v1 =  vPosibleVicoriaPC (getColumna mat (col-1))
            v2 =  vPosibleVicoriaPC (getFila mat (row-1))

-----------------------------------------------
vJugadaVictoriaPc :: String -> Int
vJugadaVictoriaPc row
     | v1 /= Nothing = (getNum v1)
     | v2 /= Nothing = (getNum v2) + 3
     | v3 /= Nothing = (getNum v3) + 2
     | v4 /= Nothing = (getNum v4) + 1
     | otherwise = -1 
     where
         v1 = getSubStringInit " OOO" row
         v2 = getSubStringInit "OOO " row 
         v3 = getSubStringInit "OO O" row 
         v4 = getSubStringInit "O OO" row 

vBloqVictoriaUser :: String -> Int
vBloqVictoriaUser row  
    | v1 /= Nothing = (getNum v1)       
    | v2 /= Nothing = (getNum v2) + 3
    | v3 /= Nothing = (getNum v3) + 2
    | v4 /= Nothing = (getNum v4) + 1
    | otherwise = -1
        where 
          v1 = getSubStringInit " XXX" row
          v2 = getSubStringInit "XXX " row
          v3 = getSubStringInit "XX X" row
          v4 = getSubStringInit "X XX" row
          -- | v5 /= Nothing = (getNum v5)
          --v5 = getSubStringInit " XX " row

vPosibleVicoriaPC :: String -> Int
vPosibleVicoriaPC row 
    | v1 /= Nothing = (getNum v1)
    | v2 /= Nothing = (getNum v2) + 1 
    | v3 /= Nothing = (getNum v3) + 2
    | otherwise = -1
    where 
         v1 = getSubStringInit " OO" row
         v2 = getSubStringInit "OO " row 
         v3 = getSubStringInit " O O" row
--------------------------------------------------