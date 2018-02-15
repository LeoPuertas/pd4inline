
module CuatroEnLinea.Constantes where


import Data.Matrix

import CuatroEnLinea.Tipos 


paramPruebas = Params { matriz = fromLists ["*********","*O      *","*X      *","*X      *","*O X    *","*XXOX  O*","*XOXXOOO*","*1234567*"],
                           nroTiro = 13,ultimoTiro = (5,4), ultimoTiroPc = (5,4), perdido = 0}

paramPorDefecto = Params { matriz = fromLists ["*********","*       *","*       *","*       *","*       *","*       *","*       *","*1234567*"],
                           nroTiro = 0, ultimoTiro = (5,8), ultimoTiroPc = (1,1), perdido = 0}


paramPruebas2 = Params { matriz = fromLists [
"*********", --1
"*        *", --2
"*        *", --3
"*OO      *", --4
"*XXX     *", --5
"*XXXOOXO *", --6
"*XXXOXOOO*", --7
"*1234567*"],
                           nroTiro = 5,ultimoTiro = (4,4), ultimoTiroPc = (5,5), perdido = 0}

mensaje :: Int ->IO()
mensaje 1 = putStrLn "La columna esta llena, selecciona otra columna donde colorar la ficha"
mensaje _ = putStrLn "Selecciona columna donde se colocara la ficha"

bloqueoVertical :: String
bloqueoVertical = "XOOO"   

vacio :: Char
vacio = ' '
