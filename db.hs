module DB where

import IO

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

remID :: Int -> Database -> Database
remID ii [] = []
remID ii ((Table i seats desc res):xs) = if ii == i then xs else (Table i seats desc res):(remID ii xs)

getSeats :: Table -> Int
getSeats (Table _ seats _ _) = seats

getID :: Table -> Int
getID (Table id _ _ _) = id

validateUniquenessOfTable :: Database -> Int -> Bool
validateUniquenessOfTable db id = 0 == length (filter ((== id).getID) db)

showTable :: Table -> String
showTable (Table i seats desc []) = "ID: " ++ show i ++ " Seats: " ++ show seats ++ " Desc: " ++ desc ++ "\n";

showDB :: Database -> String
showDB [] = "Empty"
showDB (x:xs) = (showTable x) ++ (showDB xs)


--addTab :: Table -> Database -> IO Database
--addTab t db = do return (t:db);
{-
showTab :: Database -> IO Database
--showTab [] = do putStrLn "Empty"
showTab db = do putStrLn (showDB db); return db
-}

saveTables :: [Table] -> String
--saveTable (Table i seats desc []) = show i ++ " " ++ show seats ++ " " ++ desc ++ "\n";
saveTables [] = []
saveTables ((Table i seats desc []):xs) = show i ++ " " ++ show seats ++ " " ++ desc ++ "\n" ++ (saveTables xs);

saveDB :: Database -> FilePath -> IO ()
saveDB db path = do
	h <- openFile path WriteMode
	
	hPutStr h (saveTables db)
	
	hClose h
	return ()


--readTable line = (table (read (fst (head lex line))) 666 "aaa")

parseTable line = (table ((fst i)) ((fst seats)) (fst desc) ) where
		i = (head (reads line))
		seats = (head (reads (snd i)))
		desc = (head (lex (snd seats)))

parseDB :: String -> Database
parseDB [] = []
parseDB str = (parseTable line):parseDB rest
	where
		line = fst (break ('\n'==) str)
		rest = tail (snd (break ('\n'==) str))

loadDB :: FilePath -> IO Database
loadDB path = do
	h <- openFile path ReadMode
	
--	line <- (hGetLine h)
--	putStrLn (readTable (hGetLine h))
--	putStrLn (show ( fuuu( head(lex line))))
--	putStrLn (show (fuuu line))
	cont <- hGetContents h
	putStrLn cont
	putStrLn (showDB (parseDB cont))
	hClose h
	return (parseDB cont);
	
