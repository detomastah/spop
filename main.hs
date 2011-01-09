module Main where

import DB
import Prelude
import IO
import Random

{-
main = do
	n <- randomRIO (1::Int, 100)
	putStrLn "Zgadnij o jakiej liczbie mysle (1..100):"
	doGuessing n

doGuessing n = do
	putStrLn "Wpisz liczbe:"
	w <- getLine
	let x = read w
	if x < n then do putStrLn "Za malo!"
		doGuessing n
	else if x > n then do putStrLn "Za duzo!"
		doGuessing n
	else putStrLn "Brawo!"
-}

rInt :: String -> Int
rInt n = (read n)

main = do
	menuMain []

fuu :: String -> String
fuu n = "crap" ++ n

menuMain db = do
	putStrLn "Menu: Main"
	putStrLn "\t1 - Tables"
	putStrLn "\t2 - Reservations"
	putStrLn "\tq - Quit"
	q <- getLine
	case q of
		"1"		->	do db <- menuTables db; menuMain db
		"2"		->	do menuReserv
		"q"		->	do putStrLn "Bye Bye";
		otherwise	->	do putStrLn "Invalid option"; do menuMain db


menuTables :: Database -> IO Database
menuTables db = do
	putStrLn "Menu: Tables"
	putStrLn "\t1 - Add"
	putStrLn "\t2 - Modify"
	putStrLn "\t3 - Delete"
	putStrLn "\t4 - Show"
	putStrLn "\tb - Back"
	q <- getLine
	case q of
		"1"		->	do db <- actTablesAdd db; return db;
		"2"		->	do actTablesMod; return db;
		"3"		->	do actTablesDel; return db;
		"4"		->	do showTab db; return db;
		"b"		->	do return db
		otherwise	->	do putStrLn "Invalid option"; do menuTables db


actTablesAdd :: Database -> IO Database
actTablesAdd db = do
	putStrLn "Table Add"
	putStr "Table ID: "; hFlush stdout
	i <- getLine
--	if fuu line == "aa" then putStrLn "good" else putStrLn "baad"
	
	putStr "Table number of seats: "; hFlush stdout
	seats <- getLine
--	seats :: String
	
	putStr "Description: "; hFlush stdout
	desc <- getLine
	
--	db = insert (id, seats, desc) db
	
--	addTable (table (read i) (read seats) "fuuu") []
	db <- addTab (table (read i) (read seats) "fuuu") db
	showTab db
	
	putStrLn "Done"
	return db

actTablesMod = do
	putStrLn "Table Modify"
	putStr "Table ID: "; hFlush stdout
	line <- getLine
--	id <- line
	
--	tab <- getByID id db
--	db = remove id db
	
	putStr "Table number of seats: "; hFlush stdout
	line <- getLine
--	seats <- if line == "" then getSeats tab else line
	
	putStr "Description: "; hFlush stdout
	line <- getLine
--	desc <- if line == "" then getDesc tab else line
	
	
--	db = insert (id, seats, desc) db
	
	putStrLn "Done"

actTablesDel = do
	putStrLn "Table Delete"
	putStr "Table ID: "; hFlush stdout
	line <- getLine
--	id <- line
	
--	db = remove id db
	
	putStrLn "Done"


menuReserv = do
	putStrLn "Menu: Reservations"
	putStrLn "\t1 - Add"
	putStrLn "\t2 - Modify"
	putStrLn "\t3 - Delete"
	putStrLn "\t4 - Search by ID"
	putStrLn "\tb - Back"
	q <- getLine
	case q of
		"1"		->	do putStrLn "blabla 1"; 
		"2"		->	do putStrLn "blabla 1";
		"3"		->	do putStrLn "blabla 1"; 
		"4"		->	do putStrLn "blabla 1"; 
		"b"		->	do putStrLn "blabla 1"; 
		otherwise	->	do putStrLn "Invalid option"; do menuReserv
