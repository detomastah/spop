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

menuMain :: TableList -> IO ()
menuMain tl = do
    putStrLn "\nMenu: Main"
    putStrLn "\t1 - Tables"
    putStrLn "\t2 - Reservations"
    putStrLn "\t3 - Load"
    putStrLn "\t4 - Save"
    putStrLn "\t` - Quit"
    putStr "Command? "; hFlush stdout
    q <- getLine
    case q of
        "1"		->	do tl <- menuTables tl;			menuMain tl;
        "2"		->	do menuReserv;				menuMain tl;
        "3"		->	do tl <- loadDB "database";		menuMain tl;
        "4"		->	do saveDB tl "database";			menuMain tl;
        "`"		->	do putStrLn "Bye Bye";
        otherwise	->	do putStrLn "Invalid option";		menuMain tl;



menuTables :: TableList -> IO TableList
menuTables tl = do
    putStrLn "\nMenu: Tables"
    putStrLn "\t1 - Add"
    putStrLn "\t2 - Modify"
    putStrLn "\t3 - Delete"
    putStrLn "\t4 - Show"
    putStrLn "\t` - Back"
    putStr "Command? "; hFlush stdout
    q <- getLine
    tl <- case q of
        "1"		->	do tl <- actTablesAdd tl;		menuTables tl;
        "2"		->	do tl <- actTablesMod tl;		menuTables tl;
        "3"		->	do tl <- actTablesDel tl;		menuTables tl;
        "4"		->	do putStrLn (showDB tl);		menuTables tl;
        "`"		->	do return tl;
        otherwise	->	do putStrLn "Invalid option";		menuTables tl;
    return tl;

askForID :: TableList -> Bool -> IO Int
askForID tl new = do
    i <- askForNumericValue "Table ID:"
    if validateExistenceOfTable tl i == new then return i
        else do
            putStrLn ("Error ID " ++ if new then "already exists" else "doesn't exist");
            askForID tl new;

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

actTablesAdd :: TableList -> IO TableList
actTablesAdd tl = do
    putStrLn "\nTable Add"
    id <- (askForID tl True)
    seats <- askForNumericValue "Table number of seats: "
    desc <- askForStringValue "Description: "
    return (addTable (table id seats desc) tl);

actTablesMod :: TableList -> IO TableList
actTablesMod tl = do
    putStrLn "\nTable Modify"
    id <- (askForID tl False)
    seats <- askForNumericValue "Table number of seats: "
    desc <- askForStringValue "Description: "
    return (addTable (table id seats desc) (remID id tl))

actTablesDel :: TableList -> IO TableList
actTablesDel tl = do
    putStrLn "\nTable Delete"
    id <- askForNumericValue "TableID: "
    if validateExistenceOfTable tl id == False then do 
            putStrLn "Table removed"; return (remID id tl)
        else do
            putStrLn ("Error ID doesn't exist");
            return tl;



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


actReservAdd :: TableList -> IO TableList
actReservAdd tl = do
    putStrLn "\nBrutal Reserv Add"
    id <- (askForID tl True)
    seats <- askForNumericValue "Table number of seats: "
    desc <- askForStringValue "Description: "
    return (addTable (table id seats desc) tl);






