
module CuatroEnLinea.Funciones where

import Data.List as L
import Text.Read
import System.Console.ANSI     


getSubStringInit :: String -> String -> Maybe Int
getSubStringInit _ []  = Nothing
getSubStringInit sub str = 
	if (isPrefixOf sub str)
	  then fmap (+1) $ getSubStringInit sub (tail str)
      else Just 0

getNum :: Maybe Int -> Int
getNum Nothing = -1
getNum (Just n) = n

checkInput :: String -> Bool
checkInput i 
            | (num < 8 && num > 0) = True
            | otherwise = False
            where num = getNum(readMaybeInt i)


readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

limpiarPantalla :: IO ()
limpiarPantalla = clearScreen