
module CuatroEnLinea.Funciones where

import Data.List as L
import Text.Read
import System.Console.ANSI 
import Control.Applicative  
import Data.Maybe  


getSubStringInit :: String -> String -> Maybe Int
getSubStringInit _ []  = Nothing
getSubStringInit sub str = if sub `isPrefixOf` str then Just 0 else
                              (+ 1) <$> getSubStringInit sub (tail str)

--getSubStringInit' :: String -> String -> Maybe Int
--getSubStringInit' [] _  = Nothing
--getSubStringInit' str sub = if sub `isPrefixOf` str then Just 0 else
 --                             (+ 1) <$> getSubStringInit sub (tail str)

vSubString :: String -> String -> Bool
vSubString s1 s2 = isJust $ getSubStringInit s1 s2


getNum :: Maybe Int -> Int
getNum Nothing = -1
getNum (Just n) = n

checkInput :: String -> Bool
checkInput i 
            | num < 8 && num > 0 = True
            | otherwise = False
            where num = getNum(readMaybeInt i)


readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

limpiarPantalla :: IO ()
limpiarPantalla = clearScreen
