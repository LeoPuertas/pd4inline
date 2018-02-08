{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


import Data.Matrix as M
import Control.Monad
import Data.List as L

vacio = ' '

data Params = 
     Params { matriz  :: Matrix Char, 
              nroTiro :: Int, 
              ultimoTiro :: (Int , Int),
              ultimoTiroPc :: (Int, Int)} deriving (Show)   


paramPruebas = Params { matriz = fromLists ["*********","*X   X   *","* X X   *","*  X    *","*XX X   *","*       *","*       *","*********"],
                           nroTiro = 0,ultimoTiro = (5,3), ultimoTiroPc = (-1,-1)}

paramPorDefecto = Params { matriz = fromLists ["*********","*       *","*       *","*       *","*       *","*       *","*       *","*********"],
                           nroTiro = 0, ultimoTiro = (1,1), ultimoTiroPc = (-1,-1)}


prueba = fromLists ["*********","*       *","*       *","*       *","*       *","*       *","*X      *","*********"]

getSigPieza :: Params -> Char
getSigPieza (Params{nroTiro = t})
    | odd t = 'O'
    | otherwise = 'X'

getUltPieza :: Params -> Char
getUltPieza (Params{nroTiro = t})
    | odd t = 'X'
    | otherwise = 'O'

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
    | (even t)  = w {matriz = mat, nroTiro = t + 1, ultimoTiro = (row, col+1)}
    | otherwise = w {matriz = mat, nroTiro = t + 1, ultimoTiro = (row, col+1), ultimoTiroPc = (row,col+1)}
      where row = getFilaTiro w (8, (col+1))
            pieza = getSigPieza w
            mat = (setElem (pieza) ((row),(col+1)) m)


tiroPC :: Params -> Int
tiroPC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
         -- | 1 == (victoria  w) = 1
         | 20 < (verificar'PC  w) = (verificar'PC  w) - 20
         | 10 < (verificar'PC  w) = (verificar'PC  w) - 10
         | 1 <= (verificar'  w) = verificar'  w
         | 1 <= (verificar'PC  w) = (verificar'PC  w)
         | otherwise = 1

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

--------------------------------------------------------------
vertical :: Params ->Int
vertical w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
      | getElem row col mat == getElem (row+1) col mat = 1 + vertical' mat (row+1) col 
      | otherwise = 0

vertical' :: Matrix Char -> Int -> Int ->Int
vertical' mat row  col 
      | getElem row col mat == getElem (row+1) col mat = 1 + vertical' mat (row+1) col 
      | otherwise = 0


horizontal :: Params->[Char] 
horizontal w@(Params{ matriz = m, ultimoTiro = (row , col)})  = getFila m (row-1)

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
      print $verificar mat
      print m 
      print u
      print "Selecciona columna donde se colocara la ficha"
      col <- getLine 
      jugarContraJugador' $ tiro' mat (read col :: Int)

------------Juega contra PC--------------------------------------
juegaUser :: Params -> IO Params
juegaUser mat = do
         print "Selecciona columna donde se colocara la ficha"
         col <- getLine 
         jugar' $ tiro' mat (read col :: Int)

juegaPC :: Params -> IO Params
juegaPC mat@(Params{matriz = m, ultimoTiro = (row , col) , nroTiro = n}) = do
         jugar' $ tiro' mat ((tiroPC  mat))

jugar' :: Params -> IO Params
jugar' mat@(Params{matriz = m, ultimoTiro = u, nroTiro = n}) = forever $ do
      print m
      case () of _
                  | (verificar mat) -> reiniciarJuego mat
                  | (even n)  ->  juegaUser mat
                  | otherwise ->  juegaPC mat

reiniciarJuego :: Params -> IO Params
reiniciarJuego params = do
        print $"Gano el jugador " ++ [(getUltPieza params)]
        jugar' paramPorDefecto

-----------------------------INSERTO CONTROLES----------------------------------

verificar2 :: String -> Int
verificar2 row 
    | getSubStringInit "XXXX" row /= Nothing = 10
    | getSubStringInit " XXX" row /= Nothing = (getNum $getSubStringInit " XXX" row) + 1      
    | getSubStringInit "XXX " row /= Nothing =  (getNum $getSubStringInit "XXX " row) + 4
    | getSubStringInit "XX X" row /= Nothing = 10 + (getNum $getSubStringInit "XX X" row) + 3
    | getSubStringInit "X XX" row /= Nothing = 10 + (getNum $getSubStringInit "X XX" row) + 2
    | otherwise = -1

verificar' :: Params -> Int 
verificar' w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
        | (10 < verificar2 (horizontal  w)) && (getElem (row + 1) col mat /= ' ')  =  verificar2 (horizontal  w) - 10
        | (0 < verificar2 (horizontal  w))  && (10 > verificar2 (horizontal w) ) = verificar2(horizontal  w)
        | (0 < (verificar2 (diagonalD w))) && (10 > (verificar2 (diagonalI w))) = verificar2(diagonalD w)
        | (0 < (verificar2 (diagonalI w))) && (10 > (verificar2 (diagonalD w)))= verificar2(diagonalI w)
        | (vertical w /= -1) &&  (1 + vertical w == 3) = col
        | (vertical w /= -1) &&  (1 + vertical w == 4) = 1 + vertical w
        | otherwise = -1


fichaVertical :: Params -> Int
fichaVertical w@(Params{ matriz = mat, ultimoTiro = (row , col)}) = sum[ 1 | x <- [7,6..1],  getElem (x) col mat /= ' ',getElem (x) col mat /= '*']


------------------------------JUGADA AUTOMATICA POR PC

verificarPC :: String -> Int
verificarPC row 
    | getSubStringInit "OOOO" row /= Nothing = 10
    | getSubStringInit " OOO" row /= Nothing = (getNum $getSubStringInit " OOO" row) + 1      
    | getSubStringInit "OOO " row /= Nothing = 10 + (getNum $getSubStringInit "OOO " row) + 4 
    | getSubStringInit "OO O" row /= Nothing = 10 + (getNum $getSubStringInit "OO O" row) + 3
    | getSubStringInit "O OO" row /= Nothing = 10 + (getNum $getSubStringInit "O OO" row) + 2
    | getSubStringInit "OO " row /= Nothing = (getNum $getSubStringInit "OO " row) + 2
    | getSubStringInit " OO" row /= Nothing = (getNum $getSubStringInit " OO" row) + 1
    | getSubStringInit "O O" row /= Nothing = (getNum $getSubStringInit "O O" row) + 2
    | getSubStringInit " O " row /= Nothing = (getNum $getSubStringInit " O " row) + 2
    | getSubStringInit " O" row /= Nothing = (getNum $getSubStringInit " O" row) + 1
    | getSubStringInit "O " row /= Nothing = (getNum $getSubStringInit "O " row) + 2
    | otherwise = -1


verificar'PC :: Params -> Int 
verificar'PC w@(Params{ matriz = mat, ultimoTiro = (row , col)}) 
    | (10 < verificarPC (horizontal  w)) && (getElem (row + 1) col mat /= ' ')  =  verificarPC (horizontal  w) 
	| (0 < verificarPC (horizontal  w))  && (10 > verificarPC (horizontal  w) )= verificarPC(horizontal  w)
	| (0 < (verificarPC (diagonalD w))) && (10 > (verificarPC (diagonalI w))) = verificarPC(diagonalD w)
	| (0 < (verificarPC (diagonalI w))) && (10 > (verificarPC (diagonalD w)))= verificarPC(diagonalI w)
	| (vertical w /= 0) &&  (1 + vertical w == 3) = 19 + col
	| otherwise = -1

-----------------------------------------------------------------------


--comentario