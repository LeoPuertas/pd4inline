{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


import Data.Matrix as M
import Control.Monad
import Data.List as L


vacio = ' '

data Params = 
     Params { matriz  :: Matrix Char, 
              nroTiro :: Int, 
              perdido :: Int, 
              ultimoTiro :: (Int , Int),
              ultimoTiroPc :: (Int, Int)} deriving (Show)   


paramPruebas = Params { matriz = fromLists ["*********","*       *","*       *","*       *","*  X    *","*XXOX   O*","*XOXXOOO*","*1234567*"],
                           nroTiro = 13,ultimoTiro = (5,4), ultimoTiroPc = (6,4), perdido = 0}

paramPorDefecto = Params { matriz = fromLists ["*********","*       *","*       *","*       *","*       *","*       *","*       *","*1234567*"],
                           nroTiro = 0, ultimoTiro = (1,1), ultimoTiroPc = (1,1), perdido = 0}


mat = fromLists ["*********","*       *","*       *","*O      *","*X      *","*XX   O *","*XXXO OO*","*********"]


getSigPieza :: Params -> Char
getSigPieza (Params{nroTiro = t})
    | odd t = 'O'
    | otherwise = 'X'

getUltPieza :: Params -> Char
getUltPieza (Params{nroTiro = t})
    | odd t = 'X'
    | otherwise = 'O'

bloqueoVertical :: String
bloqueoVertical = "XOOO"    

cuatroPiezas :: Char -> [Char]
cuatroPiezas p = replicate 4 p

cuatroPiezas' :: Params -> [Char]
cuatroPiezas' p = replicate 4 $getUltPieza p

-----------------------------------------------
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
            vColum = columnallena w (col) 


columnallena :: Params -> Int -> Bool
columnallena w@(Params{matriz = m}) col = elem vacio ((toLists $M.transpose m)!!(col))
     

perdioTiro :: Params -> Params
perdioTiro w@(Params{nroTiro = t}) = w {nroTiro = t - 1, perdido = 1}


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
------------------------------------------------------

getSubStringInit :: String -> String -> Maybe Int
getSubStringInit _ []  = Nothing
getSubStringInit sub str = case isPrefixOf sub str of
  False -> fmap (+1) $ getSubStringInit sub (tail str)
  True  -> Just 0

getNum :: Maybe Int -> Int
getNum Nothing = -1
getNum (Just n) = n

      
----------------------------------------------------
diagonalD:: Params ->[Char]
diagonalD w@(Params {matriz = m, ultimoTiro = (y,x)}) = reverse[(getElem (y+p) (x-p) m) | p <- [-8..8], (x-p) > 0, (y+p) > 0, (x-p) < 10, (y+p) < 9]

diagonalI:: Params ->[Char]
diagonalI w@(Params {matriz = m, ultimoTiro = (y,x)})  = reverse[(getElem (y-cx) (x-cx) m) | cx<-[-8..8], (x-cx) > 0, (y-cx) > 0, (x-cx) < 9, (y-cx) < 9]
    
getFila :: Matrix Char -> Int -> [Char]
getFila mat row = (toLists mat)!!row

getColumna :: Matrix Char -> Int -> [Char]
getColumna mat col = (toLists $M.transpose mat)!!col

-------------------Verificar---------------------------------

verificar :: Params -> Bool
verificar params = (vertical || horizontal || diagonal1 || diagonal2)
        where horizontal = vHorizontal  params 
              vertical   = vVertical    params 
              diagonal1  = vDiagonalI   params 
              diagonal2  = vDiagonalD   params 

vHorizontal :: Params -> Bool 
vHorizontal w@(Params{ matriz = m, ultimoTiro = (row , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where
       x = getSubStringInit (cuatroPiezas' w) $getFila m (row-1)

vVertical :: Params -> Bool
vVertical w@(Params{ matriz = m, ultimoTiro = (row , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where 
       x = getSubStringInit (cuatroPiezas' w) $getColumna m (col-1)




vBloquoVertical:: Params -> Bool
vBloquoVertical w@(Params{ matriz = m, ultimoTiro = (row , col)}) 
    | (x /= Nothing) = True 
    | otherwise = False
    where 
       x = getSubStringInit (bloqueoVertical)  $getColumna m (col-1)


vDiagonalD :: Params -> Bool
vDiagonalD params 
     | d /= Nothing = True
     | otherwise = False
     where d = getSubStringInit (cuatroPiezas' params) (diagonalD params)

vDiagonalI :: Params -> Bool
vDiagonalI params
     | d /= Nothing =True
     | otherwise = False
     where d =  getSubStringInit (cuatroPiezas' params) (diagonalI params)


-------------------------------------------------------------

vertical :: Params ->Int
vertical w@(Params{ matriz = mat, ultimoTiro = (row , col),ultimoTiroPc = (rowPC,colPC),nroTiro = n}) 
      | getUltPieza w == getElem (row + 1) col mat = 1 + vertical' mat (row) (col) (getUltPieza w)
      | otherwise = 0

vertical2 :: Params ->Int
vertical2 w@(Params{ matriz = mat, ultimoTiro = (row , col),ultimoTiroPc = (rowPC,colPC),nroTiro = n}) 
      | getSigPieza w == getElem (rowPC +1) colPC mat = 1 + vertical' mat (rowPC) (colPC) (getSigPieza w)
      | otherwise = 0

vertical' :: Matrix Char -> Int -> Int->Char ->Int
vertical' mat row  col pieza
      | pieza == getElem (row+1) col mat = 1 + vertical' mat (row+1) col pieza
      | otherwise = 0


horizontal :: Params->[Char] 
horizontal w@(Params{ matriz = m,ultimoTiro = (row , col),nroTiro = n}) = getFila m (row-1)


horizontal2 :: Params->[Char] 
horizontal2 w@(Params{ matriz = m,ultimoTiroPc = (rowPC,colPC),nroTiro = n}) = getFila m (rowPC-1)



-------------Modos Juego--------------------------------------

jugarContraPc :: IO Params
jugarContraPc = do
       jugar' paramPorDefecto 


jugarContraJugador :: IO Params
jugarContraJugador = do
      jugarContraJugador' paramPorDefecto

------------Juega contra Jugador---------------------------------

jugarContraJugador' :: Params -> IO Params
jugarContraJugador' mat@(Params{matriz = m, ultimoTiro = u, nroTiro = n}) = forever $ do
      print m
      case () of _ 
                   | (verificar mat) -> reiniciarJuego mat False
                   | otherwise -> do  print "Selecciona columna donde se colocara la ficha"
                                      col <- getLine 
                                      jugarContraJugador' $ tiro' mat (read col :: Int)
      

------------Juega contra PC--------------------------------------
juegaUser :: Params -> IO Params
juegaUser mat@(Params{perdido = p, ultimoTiro =u,nroTiro = n}) = do
         mensaje p
         col <- getLine 
         jugar' $ tiro' mat (read col :: Int)


mensaje :: Int ->IO()
mensaje x = do
             case () of _
                          |(x == 1)  -> print " Perdiste el turno anterior aprovecha este y elige bien la columna:"
                          |otherwise -> print "Selecciona columna donde se colocara la ficha"



juegaPC :: Params -> IO Params
juegaPC mat = do
         jugar' $ tiro' mat (tiroPC  mat)

jugar' :: Params -> IO Params
jugar' mat@(Params{matriz = m, nroTiro = n}) = forever $ do
      print m
      case () of _
                  | (verificar mat) -> reiniciarJuego mat True
                  | (even n)  ->  juegaUser mat
                  | otherwise -> juegaPC mat
                  
reiniciarJuego :: Params -> Bool -> IO Params
reiniciarJuego params vsPc = do
        print $"Gano el jugador " ++ [(getUltPieza params)]
        case () of _ 
                     | vsPc ->  jugar' paramPorDefecto
                     | otherwise -> jugarContraJugador' paramPorDefecto

-----------------------------INSERTO CONTROLES----------------------------------

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
  | (vertical2 w /= 0) &&  ( vertical2 w == 3) && ( not (vBloquoVertical w)) && (columnallena w (col)) = 19 + col
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
  | (vertical2 w /= 0) &&  (vertical2 w == 3) && ( not (vBloquoVertical w)) && (columnallena w (col)) = 19 + col
  | otherwise = -1
        where 
             verificaH = verificarPC (horizontal2  w)
             verificaD = verificarPC (diagonalD w)
             verificaI = verificarPC (diagonalI w)


verificaFilayColumna:: Matrix Char -> Int ->Int->Int
verificaFilayColumna mat row col 
          | v1 > 20 = col
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

-----------------------------------------------------------------------
