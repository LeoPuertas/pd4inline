
module CuatroEnLinea.Funciones where

import Data.List as L
import Text.Read
import System.Console.ANSI -- cabal install ansi-terminal     


getSubStringInit :: String -> String -> Maybe Int
getSubStringInit _ []  = Nothing
getSubStringInit sub str = case isPrefixOf sub str of
  False -> fmap (+1) $ getSubStringInit sub (tail str)
  True  -> Just 0

getNum :: Maybe Int -> Int
getNum Nothing = -1
getNum (Just n) = n

checkInput :: String -> Bool
checkInput i 
            | num /= Nothing = case () of _
                                           | getNum num > 7 -> False
                                           | getNum num < 1 -> False
                                           | otherwise -> True 
            | otherwise = False
          where num = readMaybeInt i

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

limpiarPantalla :: IO ()
limpiarPantalla = clearScreen