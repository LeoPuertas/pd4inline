
module CuatroEnLinea.Constantes where


import Data.Matrix

import CuatroEnLinea.Tipos 


paramPruebas = Params { matriz = fromLists ["*********","*O      *","*X      *","*X      *","*O X    *","*XXOX  O*","*XOXXOOO*","*1234567*"],
                           nroTiro = 13,ultimoTiro = (5,4), ultimoTiroPc = (5,4), perdido = 0}

paramPorDefecto = Params { matriz = fromLists ["*********","*       *","*       *","*       *","*       *","*       *","*       *","*1234567*"],
                           nroTiro = 0, ultimoTiro = (5,8), ultimoTiroPc = (1,1), perdido = 0}


paramPruebas2 = Params { matriz = fromLists [
"*********", --1
"*       *", --2
"*       *", --3
"*       *", --4
"*       *", --5
"* X    O*", --6
"*XX    O*", --7
"*1234567*"],
                           nroTiro = 5,ultimoTiro = (6,3), ultimoTiroPc = (6,8), perdido = 0}

mensaje :: Int ->IO()
mensaje x = do
             case () of _
                          |(x == 1)  -> putStrLn "Perdiste el turno anterior aprovecha este y elige bien la columna:"
                          |otherwise -> putStrLn "Selecciona columna donde se colocara la ficha"

bloqueoVertical :: String
bloqueoVertical = "XOOO"   

vacio :: Char
vacio = ' '
