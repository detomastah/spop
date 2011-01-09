module Main where

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


--db :: DBS


rInt :: String -> Int
rInt n = (read n)

main = do
	menuMain

menuMain = do
	putStrLn "Menu: Main"
	putStrLn "\t1 - Tables"
	putStrLn "\t2 - Reservations"
	putStrLn "\tq - Quit"
	q <- getLine
	case q of
		"1"		->	do menuTables
		"2"		->	do menuReserv
		"q"		->	do putStrLn "Bye Bye";
		otherwise	->	do putStrLn "Invalid option"; do menuMain


menuTables = do
	putStrLn "Menu: Tables"
	putStrLn "\t1 - Add"
	putStrLn "\t2 - Modify"
	putStrLn "\t3 - Delete"
	putStrLn "\tb - Back"
	q <- getLine
	case q of
		"1"		->	do actTablesAdd; menuMain
		"2"		->	do actTablesMod; menuMain
		"3"		->	do actTablesDel; menuMain
		"b"		->	menuMain
		otherwise	->	do putStrLn "Invalid option"; do menuTables


actTablesAdd = do
	putStrLn "Table Add"
	putStr "Table ID: "; hFlush stdout
	line <- getLine
--	i <- (read line)
	putStr "Table number of seats: "; hFlush stdout
	line <- getLine
--	seats <- (read line)
	
	putStr "Description: "; hFlush stdout
	line <- getLine
--	desc <- "a" ++ line
	
--	db = insert (id, seats, desc) db
	
	putStrLn "Done"

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
		"b"		->	menuMain
		otherwise	->	do putStrLn "Invalid option"; do menuReserv
