module DB where

import IO
import System.Time
import Char

{- id stolika, nazwisko, dzien, godzina, uwagi -}
data Reservation = Reservation String String String String deriving(Show, Read)

{- numer stolika, ilosc siedzen, opis, rezerwacje -}
data Table = Table Int Int String [Reservation] deriving(Show, Read)

type TableList = [Table]

table :: Int -> Int -> String -> Table
table i seats desc = Table i seats desc []

addTable :: Table -> TableList -> TableList
addTable t [] = [t]
addTable t tl = t:tl

remID :: Int -> TableList -> TableList
remID ii [] = []
remID ii ((Table i seats desc res):xs) = if ii == i then xs else (Table i seats desc res):(remID ii xs)

getSeats :: Table -> Int
getSeats (Table _ seats _ _) = seats

getID :: Table -> Int
getID (Table id _ _ _) = id

validateExistenceOfTable :: TableList -> Int -> Bool
validateExistenceOfTable tl id = 0 == length (filter ((== id).getID) tl)

validateNumericalityOf str = foldr (&&) True (map isDigit str) 

showTable :: Table -> String
showTable (Table i seats desc []) = "ID: " ++ show i ++ " Seats: " ++ show seats ++ " Desc: " ++ desc ++ "\n";

showDB :: TableList -> String
showDB [] = "Empty"
showDB (x:xs) = (showTable x) ++ (showDB xs)

saveDB :: TableList -> FilePath -> IO ()
saveDB tl path = do
	h <- openFile (concat [path, ".tables"]) WriteMode
	hPutStr h (show tl)
	hClose h
	return ()

parseTable line = (table ((fst i)) ((fst seats)) (fst desc) ) where
		i = (head (reads line))
		seats = (head (reads (snd i)))
		desc = (head (lex (snd seats)))

parseDB :: String -> TableList
parseDB [] = []
parseDB str = (parseTable line):parseDB rest
	where
		line = fst (break ('\n'==) str)
		rest = tail (snd (break ('\n'==) str))

loadDB :: FilePath -> IO TableList
loadDB path = do
	h <- openFile (concat [path, ".tables"]) ReadMode
	cont <- hGetContents h
	return $! (read cont);
	
