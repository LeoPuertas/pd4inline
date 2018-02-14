{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module CuatroEnLinea where

import CuatroEnLinea.Tipos
import CuatroEnLinea.Constantes
import CuatroEnLinea.Piezas
import CuatroEnLinea.Funciones
import CuatroEnLinea.Verificaciones
import CuatroEnLinea.Jugadas
import CuatroEnLinea.Grafico

import System.Exit
-------------Modos Juego--------------------------------------
main :: IO Params 
main = do 
        putStrLn "Inicio de Juego Cuatro en Linea"
        putStrLn "Modos de Juego"
        putStrLn "1. 1 Jugador"
        putStrLn "2. 2 Jugadores"
        putStrLn "3. Salir"
        opc <- getLine
        case opc of _ 
                     | opc == "1" -> jugarContraPc
                     | opc == "2" -> jugarContraJugador
                     | otherwise  -> exitSuccess



jugarContraPc :: IO Params
jugarContraPc = do
       limpiarPantalla
       putStrLn "Inicio de juego vs Pc"
       jugar' paramPorDefecto 


jugarContraJugador :: IO Params
jugarContraJugador = do
      limpiarPantalla
      putStrLn "Inicio de juego vs Jugador"
      jugarContraJugador' paramPorDefecto

------------Juega contra Jugador---------------------------------

jugarContraJugador' :: Params -> IO Params
jugarContraJugador' mat@(Params{matriz = m, ultimoTiro = u, nroTiro = n}) = do
      case () of _ 
                   | (vVictoria mat) -> reiniciarJuego mat
                   | lleno mat -> empate mat
                   | otherwise -> do  print m
                                      putStrLn "Selecciona columna donde se colocara la ficha"
                                      col <- getLine 
                                      limpiarPantalla
                                      case (checkInput col) of True -> jugarContraJugador' $ tiro' mat (read col :: Int) 
                                                               otherwise -> do putStrLn "Error de Jugada, intente de nuevo"
                                                                               jugarContraJugador' mat

------------Juega contra PC--------------------------------------

juegaUser :: Params -> IO Params
juegaUser mat@(Params{matriz = m, perdido = p, ultimoTiro =u,nroTiro = n}) = do
         mensaje p
         col <- getLine 
         case (checkInput col) of True -> jugar' $ tiro' mat (read col :: Int) 
                                  otherwise ->  do putStrLn "Error de Jugada, intente de nuevo"
                                                   jugar' mat

juegaPC :: Params -> IO Params
juegaPC mat = do
         limpiarPantalla
         jugar' $ tiro' mat (tiroPC  mat)

jugar' :: Params -> IO Params
jugar' mat@(Params{matriz = m, nroTiro = n}) = do
      print m
      case () of _
                  | vVictoria mat -> reiniciarJuego mat
                  | (even n)  ->  juegaUser mat
                  | otherwise -> juegaPC mat
                  
-------------------------------------------------------------------

reiniciarJuego :: Params  -> IO Params
reiniciarJuego params = do
        limpiarPantalla  
        print (matriz params)
        mostrarTablero params
        putStrLn $"Gano el jugador " ++ [(getUltPieza params)] ++ "\n"
        main

empate :: Params -> IO Params
empate params = do
        limpiarPantalla
        print (matriz params)
        putStrLn "El juego finalizo en empate \n" 
        main