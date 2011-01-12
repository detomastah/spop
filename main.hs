module Main where

import DB
import Prelude
import IO
import Random


rInt :: String -> Int
rInt n = (read n)

main = do
	menuMain []

fuu :: String -> String
fuu n = "crap" ++ n

menuMain :: Database -> IO ()
menuMain db = do
	putStrLn "\nMenu: Main"
	putStrLn "\t1 - Tables"
	putStrLn "\t2 - Reservations"
	putStrLn "\t3 - Load"
	putStrLn "\t4 - Save"
	putStrLn "\t` - Quit"
	putStr "Command? "; hFlush stdout
	q <- getLine
	case q of
		"1"		->	do db <- menuTables db;			menuMain db;
		"2"		->	do menuReserv;				menuMain db;
		"3"		->	do db <- loadDB "heh.cdb";		menuMain db;
		"4"		->	do saveDB db "heh.cdb";			menuMain db;
		"`"		->	do putStrLn "Bye Bye";
		otherwise	->	do putStrLn "Invalid option";		menuMain db;



menuTables :: Database -> IO Database
menuTables db = do
	putStrLn "\nMenu: Tables"
	putStrLn "\t1 - Add"
	putStrLn "\t2 - Modify"
	putStrLn "\t3 - Delete"
	putStrLn "\t4 - Show"
	putStrLn "\t` - Back"
	putStr "Command? "; hFlush stdout
	q <- getLine
	db <- case q of
		"1"		->	do db <- actTablesAdd db;		menuTables db;
		"2"		->	do db <- actTablesMod db;		menuTables db;
		"3"		->	do db <- actTablesDel db;		menuTables db;
		"4"		->	do putStrLn (showDB db);		menuTables db;
		"`"		->	do return db;
		otherwise	->	do putStrLn "Invalid option";		menuTables db;
	return db;

askForID :: Database -> Bool -> IO Int
askForID db new = do
	putStr "Table ID: "; hFlush stdout;
	i <- getLine;
	if validateUniquenessOfTable db (read i) == new then
			return (read i)
		else do
			putStrLn ("Error ID " ++ if new then "already exists" else "doesn't exist");
			askForID db new;

actTablesAdd :: Database -> IO Database
actTablesAdd db = do
	putStrLn "\nTable Add"
	
	i <- (askForID db True)
	
	putStr "Table number of seats: "; hFlush stdout
	seats <- getLine
	
	putStr "Description: "; hFlush stdout
	desc <- getLine
	
	--moze byc tak
	return (addTable (table (i) (read seats) desc) db)
	
{-	--albo tak
	db <- addTab (table (read i) (read seats) desc) db
	return db
	-}

actTablesMod :: Database -> IO Database
actTablesMod db = do
	putStrLn "\nTable Modify"
	
	i <- (askForID db False)
	
	putStr "Table number of seats: "; hFlush stdout
	seats <- getLine
	
	putStr "Description: "; hFlush stdout
	desc <- getLine
	
	return (addTable (table i (read seats) desc) (remID i db))

actTablesDel :: Database -> IO Database
actTablesDel db = do
	putStrLn "\nTable Delete"
	
	putStr "Table ID: "; hFlush stdout;
	i <- getLine;
	if validateUniquenessOfTable db (read i) == False then
			do putStrLn "Ok removing"; return (remID (read i) db)
		else do
			putStrLn ("Error ID doesn't exist");
			return db;



menuReserv = do
	putStrLn "\nMenu: Reservations"
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
