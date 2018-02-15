
module CuatroEnLinea.Constantes where

import Data.Matrix
import CuatroEnLinea.Tipos 

paramPorDefecto :: Params
paramPorDefecto = Params { matriz = fromLists ["*********","*       *","*       *","*       *","*       *","*       *","*       *","*1234567*"],
                           nroTiro = 0, ultimoTiro = (5,8), ultimoTiroPc = (1,1), perdido = 0}

mensaje :: Int ->IO()
mensaje 1 = putStrLn "La columna esta llena, selecciona otra columna donde colorar la ficha"
mensaje _ = putStrLn "Selecciona columna donde se colocara la ficha"

bloqueoVertical :: String
bloqueoVertical = "XOOO"   

vacio :: Char
vacio = ' '
