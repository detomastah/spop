module DB where

import System.Time

{- nazwisko, dzien, godzina, uwagi -}
data Reservation = Reservation String String String String deriving Show
{- numer stolika, ilosc siedzen, opis, rezerwacje -}
data Table = Table Int Int String [Reservation] deriving Show

type Database = [Table]

table :: Int -> Int -> String -> Table
table i seats desc = Table i seats desc []

addTable :: Table -> Database -> Database
addTable t [] = [t]
addTable t db = t:db

showTable :: Table -> String
showTable (Table i seats desc []) = "ID: " ++ show i ++ " Seats: " ++ show seats ++ " Desc: " ++ desc ++ "\n";

showDB :: Database -> String
showDB [] = "Empty"
showDB (x:xs) = (showTable x) ++ (showDB xs)


addTab :: Table -> Database -> IO Database
addTab t db = do return (t:db);
{-
showTab :: Database -> IO Database
--showTab [] = do putStrLn "Empty"
showTab db = do putStrLn (showDB db); return db
-}