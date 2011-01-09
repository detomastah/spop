import System.Time

{- nazwisko, dzien, godzina, uwagi -}
data Reservation = Reservation String String String String deriving Show
{- numer stolika, ilosc siedzen, opis, rezerwacje -}
data Table = Table Int Int String [Reservation] deriving Show

type Database = [Table]

table :: Int -> Int -> String -> Table
table id seats desc = Table id seats desc []

addTable :: Table -> Database -> Database
addTable t [] = [t]
addTable t db = t:db

