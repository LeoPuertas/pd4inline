
module CuatroEnLinea.Piezas where

import CuatroEnLinea.Tipos 


getSigPieza :: Params -> Char
getSigPieza Params{nroTiro = t}
    | odd t = 'O'
    | otherwise = 'X'

getUltPieza :: Params -> Char
getUltPieza Params{nroTiro = t}
    | odd t = 'X'
    | otherwise = 'O'

cuatroPiezas :: Params -> String
cuatroPiezas p = replicate 4 $getUltPieza p
