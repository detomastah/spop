module Main where

import DB
import Prelude
import IO
import System.Time
import Random


rInt :: String -> Int
rInt n = (read n)

main = do
    db <- loadDB "database"
    menuMain db

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
        "1"		->	do tl <- menuTables tl;             menuMain tl;
        "2"		->	do tl <- menuReserv tl;             menuMain tl;
        "3"		->	do tl <- loadDB "database";         menuMain tl;
        "4"		->	do saveDB tl "database";            menuMain tl;
        "`"		->	do putStrLn "Bye Bye";
        otherwise	->	do putStrLn "Invalid option";   menuMain tl;



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
    i <- askForNumericValue "Table ID (0 - cancel):"
    if (i == 0) || (validateExistenceOfTable tl i == new) then return i
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



askForDayTimeValue :: IO CalendarTime
askForDayTimeValue = do
    day <- askForNumericValue "Day: "
    hour <- askForNumericValue "Hour: "
    minu <- askForNumericValue "Minutes (rounded down to 15 minutes): "
    minu <- return ((div minu 15) * 15)
    cur <- getClockTime
    cal <- toCalendarTime cur
    if day >= (calGetDay cal) then 
            return (toUTCTime (toClockTime (CalendarTime (calGetYear cal) (toEnum (calGetMonth cal)) day hour minu 0 0 Monday 0 "" 0 False)))
        else
            return (toUTCTime (toClockTime (CalendarTime (calGetYear cal) (toEnum (calGetMonth cal+1)) day hour minu 0 0 Monday 0 "" 0 False)))

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
    return (addTable (table id seats desc) (remById id tl))

actTablesDel :: TableList -> IO TableList
actTablesDel tl = do
    putStrLn "\nTable Delete"
    id <- askForNumericValue "TableID: "
    if validateExistenceOfTable tl id == False then do 
            putStrLn "Table removed"; return (remById id tl)
        else do
            putStrLn ("Error ID doesn't exist");
            return tl;



menuReserv :: TableList -> IO TableList
menuReserv tl = do
    putStrLn "\nMenu: Reservations"
    putStrLn "\t1 - Add"
    putStrLn "\t2 - Modify"
    putStrLn "\t3 - Delete by Name"
    putStrLn "\t4 - Show"
    putStrLn "\t` - Back"
    putStr "Command? "; hFlush stdout
    q <- getLine
    tl <- case q of
        "1"		->	do tl <- actReservAdd tl;           menuReserv tl;
        "2"		->	do putStrLn "blabla 1";             menuReserv tl;
        "3"		->	do  tl <- actReservDelByName tl;    menuReserv tl;
        "4"     ->  do putStrLn (showDB tl);            menuReserv tl;
        "`"		->	do return tl;
        otherwise	->	do putStrLn "Invalid option";   menuReserv tl;
    return tl;
    
    -- Wpisanie pustego daje wyjatek PARSE!!!! SKORYGOWAC!!!!

actReservAdd :: TableList -> IO TableList
actReservAdd tl = do
    putStrLn "\nReservation Add"
    
    date <- askForDayTimeValue;
    putStrLn (show date)
    i <- askForNumericValue "For how many minutes (rounded up to 15 minutes): "
    period <- return (normalizeTimeDiff (minutesPeriod ((div (i+14) 15) * 15)))
    putStrLn (show period)
    seats <- askForNumericValue "How many angry people: "
    name <- askForStringValue "Angry client's surname: "
    
    let available_tables = tablesReadyToReserve date period seats tl
    
    new_tl <- if length available_tables > 0 then do
                putStrLn "Available tables: "
                putStrLn (showDB available_tables)
                id <- askForID available_tables False
                return (addReservationToTable tl id name date period)
            else do
                    putStrLn "No available tables found"
                    return tl
    
    return new_tl;


actReservDelByName :: TableList -> IO TableList
actReservDelByName tl = do
    putStrLn "\nReservation Delete"
    
    name <- askForStringValue "Angry client's surname: "
    putStrLn (showDB (filterTablesWithReservByName tl name))
--    date <- askForDayTimeValue;
    
    putStrLn "\nMenu:"
    putStrLn "\t1 - All"
    putStrLn "\t2 - By Date"
    putStrLn "\t3 - By Table ID & Date"
    putStr "Command? "; hFlush stdout
    q <- getLine
    tl <- (case q of
        "1"     ->  return (remReservationFromTL_Name tl name)
        "2"     ->  do date <- askForDayTimeValue;
                        return (remReservationFromTL_NameDate tl name date)
        "3"     ->  do id <- askForID tl False;
                        date <- askForDayTimeValue;
                        return (if id /= 0 then (remReservationFromTL_IDDate tl id date) else tl)
        )
    
    putStrLn ("\nReservation status for " ++ name ++ ":")
    putStrLn (showDB (filterTablesWithReservByName tl name))
    
    return tl




