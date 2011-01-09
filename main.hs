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
	putStrLn "\tq - Quit"
	q <- getLine
	case q of
		"1"		->	do menuTables
		"q"		->	do putStrLn "Bye Bye";
		otherwise	->	do putStrLn "invalid option"; do menuMain


menuTables = do
	putStrLn "Menu: Tables"
	putStrLn "\t1 - Add"
	putStrLn "\t2 - Modify"
	putStrLn "\t3 - Delete"
	putStrLn "\tb - Back"
	q <- getLine
	case q of
		"1"		->	do putStrLn "blabla 1"; actTablesAdd
		"2"		->	do putStrLn "blabla 1"; menuTables
		"3"		->	do putStrLn "blabla 1"; actTablesDel
		"b"		->	menuMain
		otherwise	->	do putStrLn "invalid option"; do menuMain


actTablesAdd = do
	putStrLn "Table Add"
	putStr "Table ID: "
	line <- getLine
--	i <- (read line)
	putStr "Table number of seats: "
	line <- getLine
--	seats <- (read line)
	
	putStr "Description: "
	line <- getLine
--	desc <- "a" ++ line
	
--	db = insert (id, seats, desc) db
	
	putStrLn "Done"

actTablesMod = do
	putStrLn "Table Modify"
	putStr "Table ID: "
	line <- getLine
--	id <- line
	
--	tab <- getByID id db
--	db = remove id db
	
	putStr "Table number of seats: "
	line <- getLine
--	seats <- if line == "" then getSeats tab else line
	
	putStr "Description: "
	line <- getLine
--	desc <- if line == "" then getDesc tab else line
	
	
--	db = insert (id, seats, desc) db
	
	putStrLn "Done"

actTablesDel = do
	putStrLn "Table Delete"
	putStr "Table ID: "
	line <- getLine
--	id <- line
	
--	db = remove id db
	
	putStrLn "Done"
