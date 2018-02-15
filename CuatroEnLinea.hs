{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

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
        case opc of  
              "1" -> jugarContraPc
              "2" -> jugarContraJugador
              otherwise  -> exitSuccess



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
jugarContraJugador' mat@(Params{matriz = m, ultimoTiro = u, nroTiro = n, perdido = p})  
      | (vVictoria mat) = reiniciarJuego mat
      | lleno mat = empate mat
      | otherwise =  do print m
                        mensaje p
                        col <- getLine 
                        limpiarPantalla
                        if  (checkInput col) 
                          then  jugarContraJugador' $ tiro' mat (read col :: Int) 
                          else do 
                                  print mat
                                  putStrLn "Error de Jugada, intente de nuevo"
                                  jugarContraJugador' mat

------------Juega contra PC--------------------------------------

juegaUser :: Params -> IO Params
juegaUser mat@(Params{matriz = m, perdido = p, ultimoTiro =u,nroTiro = n}) = do
         mensaje p
         col <- getLine 
         if (checkInput col) 
           then jugar' $ tiro' mat (read col :: Int) 
           else do putStrLn "Error de Jugada, intente de nuevo"
                   jugar' mat

juegaPC :: Params -> IO Params
juegaPC mat = do
         limpiarPantalla
         jugar' $ tiro' mat (tiroPC  mat)

jugar' :: Params -> IO Params
jugar' mat@(Params{matriz = m, nroTiro = n}) = do
      print m
      if(vVictoria mat) 
        then reiniciarJuego mat
        else if (even n) 
              then  juegaUser mat
              else  juegaPC mat
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