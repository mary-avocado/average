module Main where

import Data.Fixed

main :: IO ()
main = 
  putStrLn "Сколько дней" >>
  (readLn :: IO Integer) >>=
  \дни -> 
    getTemperatures дни >>=
    \ts -> 
      putStrLn "Средняя температура" >>
      putStrLn (show (avg ts) ++"°C") >>
      putStrLn "Максимальная температура" >>
      putStrLn (show (maximum ts) ++ "°C")  >>
      putStrLn "Минимальная температура" >>
      putStrLn (show (minimum ts) ++ "°C")
 
getTemperatures :: Integer -> IO [Centi]
getTemperatures =
 \days -> 
   if days == 0
   then return []
   else  putStrLn "Какая температура °C" >>
   (readLn :: IO Centi) >>=
     \t ->
     getTemperatures (days - 1) >>=
     \ts ->
     return (t : ts)

avg :: [Centi] -> Centi
avg = \xs -> sum xs / fromIntegral (length xs) 


