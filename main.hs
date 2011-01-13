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
    i <- askForNumericValue "Table ID:"
    if validateExistenceOfTable db i == new then return i
        else do
	        putStrLn ("Error ID " ++ if new then "already exists" else "doesn't exist");
	        askForID db new;
			    
askForNumericValue :: String -> IO Int 			    
askForNumericValue question = do
    putStr question; hFlush stdout 
    i <- getLine
    if validateNumericalityOf i == False then do
        putStrLn "Invalid value: non numeric"
        askForNumericValue question
        else return (read i)
        
askForStringValue :: String -> IO String
askForStringValue question = do
    putStr question; hFlush stdout 
    i <- getLine
    return i

actTablesAdd :: Database -> IO Database
actTablesAdd db = do
	putStrLn "\nTable Add"
	id <- (askForID db True)
	seats <- askForNumericValue "Table number of seats: "
	desc <- askForStringValue "Description: "
	return (addTable (table id seats desc) db);

actTablesMod :: Database -> IO Database
actTablesMod db = do
    putStrLn "\nTable Modify"
    id <- (askForID db False)
    seats <- askForNumericValue "Table number of seats: "
    desc <- askForStringValue "Description: "
    return (addTable (table id seats desc) (remID id db))

actTablesDel :: Database -> IO Database
actTablesDel db = do
	putStrLn "\nTable Delete"
	id <- askForNumericValue "TableID: "
	if validateExistenceOfTable db id == False then
			do putStrLn "Table removed"; return (remID id db)
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
