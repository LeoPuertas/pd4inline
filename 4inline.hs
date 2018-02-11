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
tiro' w@(Params{ matriz = m, nroTiro = t, perdido = p}) col 
    | (even t) && (columnallena w (col)) = w {matriz = (setElem (pieza) ((row),(col+1)) m), nroTiro = t + 1, ultimoTiro = (row, col+1), perdido = 0}
    | (odd t) &&  (columnallena w (col)) = w {matriz = (setElem (pieza) ((row),(col+1)) m), nroTiro = t + 1, ultimoTiro = (row, col+1), ultimoTiroPc = (row,col+1), perdido = 0}
    | otherwise = perdioTiro w
      where row = getFilaTiro w (8, (col+1))
            pieza = getSigPieza w


columnallena :: Params -> Int -> Bool
columnallena w@(Params{ matriz = m, nroTiro = t, perdido = p}) col = elem vacio ((toLists $M.transpose m)!!(col))
     

perdioTiro :: Params -> Params
perdioTiro w@(Params{ matriz = mat, ultimoTiro = (row , col), nroTiro = t, perdido = p }) = w {matriz = mat, nroTiro = t + 1, ultimoTiro = (row, col), ultimoTiroPc = (row,col), perdido = 1}


tiroPC :: Params -> Int
tiroPC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
         | 1 == (victoria  w) = 1
         | 20 < verificacion = verificacion - 20
         | 20 < verificacionPC = verificacionPC - 20
         | length(barreFilasyColumnas w) > 20 = head (barreFilasyColumnas w) - 21 --resto 1 mas porque el tiro lleva 1 columna menos
         | length(barreFilasyColumnas w) > 0 = head (barreFilasyColumnas w)
         | 1 <= (verificar'  w) = verificar'  w 
         | 10 > verificacion  && 0 < verificacion  = verificacion 
         | 10 > verificacionPC  && 0 < verificacionPC  = verificacionPC
         | 1 <= verificacion = verificacion
         | 1 <= verificacionPC = verificacionPC
         | otherwise = row
           where
                 verificacion = (verificar'PC  w)
                 verificacionPC = (verificarUltimoPC w)
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
horizontal w@(Params{ matriz = m,ultimoTiro = (row , col),ultimoTiroPc = (rowPC,colPC),nroTiro = n}) = getFila m (row-1)


horizontal2 :: Params->[Char] 
horizontal2 w@(Params{ matriz = m,ultimoTiro = (row , col),ultimoTiroPc = (rowPC,colPC),nroTiro = n}) = getFila m (rowPC-1)



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
juegaPC mat@(Params{matriz = m, ultimoTiro = (row , col) , nroTiro = n}) = do
         jugar' $ tiro' mat ((tiroPC  mat))

jugar' :: Params -> IO Params
jugar' mat@(Params{matriz = m, ultimoTiro = u, nroTiro = n}) = forever $ do
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
    | getSubStringInit "XXXX" row /= Nothing = 20
    | getSubStringInit " XX " row /= Nothing = 10 + (getNum $getSubStringInit " XX " row) 
    | getSubStringInit " XXX" row /= Nothing = 10 + (getNum $getSubStringInit " XXX" row)       
    | getSubStringInit "XXX " row /= Nothing = 10 + (getNum $getSubStringInit "XXX " row) + 3
    | getSubStringInit "XX X" row /= Nothing = 10 + (getNum $getSubStringInit "XX X" row) + 2
    | getSubStringInit "X XX" row /= Nothing = 10 + (getNum $getSubStringInit "X XX" row) + 1
    | otherwise = -1

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
    | 20 == (verificar2 (horizontal  w)) = 1 
    | 20 == (verificar2 (diagonalD w)) = 1
    | 20 == (verificar2 (diagonalI w)) = 1
    | 4 ==  vertical w = 1
    | otherwise = 0


fichaVertical :: Params -> Int
fichaVertical w@(Params{ matriz = mat, ultimoTiro = (row , col)}) = sum[ 1 | x <- [7,6..1],  getElem (x) col mat /= ' ',getElem (x) col mat /= '*']


------------------------------JUGADA AUTOMATICA POR PC

barreFilasyColumnas:: Params -> [Int] 
barreFilasyColumnas w@(Params{ matriz = mat})= [ verificaFilayColumna mat row col| row <- [7,6..1], col <- [1,3..7], (verificaFilayColumna mat row col)>0 ]



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
    | getSubStringInit "OOOO" row /= Nothing = 20
    | getSubStringInit " OOO" row /= Nothing = 20 +(getNum $getSubStringInit " OOO" row)       
    | getSubStringInit "OOO " row /= Nothing = 20 + (getNum $getSubStringInit "OOO " row) + 3 
    | getSubStringInit "OO O" row /= Nothing = 20 + (getNum $getSubStringInit "OO O" row) + 2
    | getSubStringInit "O OO" row /= Nothing = 20 + (getNum $getSubStringInit "O OO" row) + 1
    | getSubStringInit "OO " row /= Nothing = (getNum $getSubStringInit "OO " row) + 1
    | getSubStringInit " OO" row /= Nothing = (getNum $getSubStringInit " OO" row) 
    | getSubStringInit " O O" row /= Nothing = (getNum $getSubStringInit " O O" row) + 2
    | otherwise = -1





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
                    |(verificarPC (getColumna mat (col-1))) > 20 = col
					|((verificarPC (getFila mat (row-1)) ) > 20)  = verificarPC (getFila mat (row)) 
					|otherwise = -1



victoriaPC :: Params  -> Int
victoriaPC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
        | 20 == (verificarPC (horizontal  w)) = 1 
        | 20 == (verificarPC (diagonalD w)) = 1
        | 20 == (verificarPC (diagonalI w)) = 1
        | 4 == 1 + vertical w = 1
        | otherwise = 0

-----------------------------------------------------------------------
