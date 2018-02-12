module CuatroEnLinea.Tipos where

import Data.Matrix

data Params = 
     Params { matriz  :: Matrix Char, 
              nroTiro :: Int, 
              perdido :: Int, 
              ultimoTiro :: (Int , Int),
              ultimoTiroPc :: (Int, Int)} deriving (Show) 