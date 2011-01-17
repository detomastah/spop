module Main where

import DB
import Prelude
import IO
import System.Time
import Random


main = do
    db <- loadDB "database"
    menuMain db

menuMain :: TableList -> IO ()
menuMain tl = do
    putStrLn "\nMenu: Main"
    putStrLn "\t1 - Manage Tables"
    putStrLn "\t2 - Reservations"
    putStrLn "\t3 - Search for free tables by date"
    putStrLn "\t4 - Load"
    putStrLn "\t5 - Save"
    putStrLn "\t` - Quit"
    putStr "Command? "; hFlush stdout
    q <- getLine
    case q of
        "1"         ->	do tl <- menuTables tl;             menuMain tl;
        "2"         ->	do tl <- menuReserv tl;             menuMain tl;
        "3"         ->  do actSearchForFreeTablesByDate tl; menuMain tl;
        "4"         ->	do tl <- loadDB "database";         menuMain tl;
        "5"         ->	do saveDB tl "database";            menuMain tl;
        "`"         ->	do putStrLn "Bye Bye";
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
        "1"         ->	do tl <- actTablesAdd tl;		menuTables tl;
        "2"         ->	do tl <- actTablesMod tl;		menuTables tl;
        "3"         ->	do tl <- actTablesDel tl;		menuTables tl;
        "4"         ->	do putStrLn (showJustTables tl);		menuTables tl;
        "`"         ->	do return tl;
        otherwise   ->	do putStrLn "Invalid option";		menuTables tl;
    return tl;

askForID :: TableList -> Bool -> IO Int
askForID tl new = do
    i <- askForNumericValue "Table ID (0 - cancel): "
    if (i == 0) || (validateExistenceOfTable tl i == new) then return i
        else do
            putStrLn ("Error ID " ++ if new then "already exists" else "doesn't exist");
            askForID tl new;

askForNumericValue :: String -> IO Int
askForNumericValue question = do
    putStr question; hFlush stdout 
    i <- getLine
    if length i < 1 || validateNumericalityOf i == False then do
            putStrLn "Invalid value: non numeric"
            askForNumericValue question
        else return (read i)

askForStringValue :: String -> IO String
askForStringValue question = do
    putStr question; hFlush stdout 
    i <- getLine
    return i

askForNumericValueWithLimits question min max = do
    i <- askForNumericValue (question ++ "(" ++ show min ++ ", " ++ show max ++ "): ")
    if i >= min && i <= max 
        then do return i
        else do putStrLn "Invalid value: out of bounds"
                askForNumericValueWithLimits question min max


askForDayTimeValue :: IO CalendarTime
askForDayTimeValue = do
    day <- askForNumericValueWithLimits "Day: " 1 31
    hour <- askForNumericValueWithLimits "Hour: " 0 23
    minu <- askForNumericValueWithLimits "Minutes (rounded down to 15 minutes): " 0 59
    minu <- return ((div minu 15) * 15)
    cur <- getClockTime
    cal <- toCalendarTime cur
    cal <- if day >= (calGetDay cal) 
            then return (toUTCTime (toClockTime (CalendarTime (calGetYear cal) (toEnum (fromEnum (calGetMonth cal))) day hour minu 0 0 Monday 0 "" 0 False)))
            else return (let y = calGetYear cal + (div m 12); m = ((fromEnum (calGetMonth cal)) + 1)
                            in (toUTCTime (toClockTime (CalendarTime y (toEnum (mod m 12)) day hour minu 0 0 Monday 0 "" 0 False))))
            
    if calGetDay cal == day
        then return cal
        else do putStrLn ("Not enough days in month: " ++ show (calGetMonth cal)); askForDayTimeValue

askForTimePeriod = do i <- askForNumericValue "For how many minutes (rounded up to 15 minutes): "; return (normalizeTimeDiff (minutesPeriod ((div (i+14) 15) * 15)))
 
actTablesAdd :: TableList -> IO TableList
actTablesAdd tl = do
    putStrLn "\nTable Add"
    id <- (askForID tl True)
    if id == 0 then return tl
        else do
        seats <- askForNumericValue "Table number of seats: "
        desc <- askForStringValue "Description: "
        return (addTable (table id seats desc) tl);

actTablesMod :: TableList -> IO TableList
actTablesMod tl = do
    putStrLn "\nTable Modify"
    id <- (askForID tl False)
    if id == 0 then return tl
        else do
        seats <- askForNumericValue "Table number of seats: "
        desc <- askForStringValue "Description: "
        return (addTable (table id seats desc) (remById id tl))

actTablesDel :: TableList -> IO TableList
actTablesDel tl = do
    putStrLn "\nTable Delete"
    id <- (askForID tl False)
    if id == 0 then return tl
        else do 
        putStrLn "Table removed"; return (remById id tl)



menuReserv :: TableList -> IO TableList
menuReserv tl = do
    putStrLn "\nMenu: Reservations"
    putStrLn "\t1 - Add"
    putStrLn "\t2 - Modify by Name"
    putStrLn "\t3 - Delete by Name"
    putStrLn "\t4 - Delete old"
    putStrLn "\t5 - Search by Name"
    putStrLn "\t6 - Show"
    putStrLn "\t` - Back"
    putStr "Command? "; hFlush stdout
    q <- getLine
    tl <- case q of
        "1"         ->  do tl <- actReservAdd tl;           menuReserv tl;
        "2"         ->  do tl <- actReservModByName tl;     menuReserv tl;
        "3"         ->  do tl <- actReservDelByName tl;     menuReserv tl;
        "4"         ->  do cur <- getClockTime;
                            date <- (toCalendarTime cur);
                            date <- return (toUTCTime (toClockTime date));
                            tl <- (return (remReservationBeforeDate tl date));
                            menuReserv tl;
        "5"         ->  do actSearchForReservByName tl;     menuReserv tl;
        "6"         ->  do putStrLn (showDB tl);            menuReserv tl;
        "`"         ->  do return tl;
        otherwise   ->  do putStrLn "Invalid option";   menuReserv tl;
    return tl;

actReservAdd :: TableList -> IO TableList
actReservAdd tl = do
    putStrLn "\nReservation Add"
    
    date <- askForDayTimeValue;
    putStrLn (show date)
    period <- askForTimePeriod
    seats <- askForNumericValue "How many angry people: "

    
    let available_tables = tablesReadyToReserve date period seats tl
    
    new_tl <- if length available_tables > 0 then do
                putStrLn "Available tables: "
                putStrLn (showDB available_tables)
                id <- askForID available_tables False
                if id == 0 then return tl
                    else do name <- askForStringValue "Angry client's surname: "; return (addReservation tl id name date period)
            else do
                    putStrLn "No available tables found"
                    return tl
    
    return new_tl;

actReservModByName :: TableList -> IO TableList
actReservModByName tl = do
    putStrLn "\nReservation Modify"
    
    name <- askForStringValue "Angry client's surname: "
    
    let available_tables = filterTablesWithReservByName tl name
    new_tl <- if length available_tables > 0 then do
            putStrLn (showDB available_tables)
            
            id <- askForID available_tables False;
            if id == 0 then return tl
                else do
                date <- askForDayTimeValue

                if existsReservationByDate (getReservations (findTableByID tl id)) date == False then return tl
                    else do
                        new_tl <- return (remReservationByIDAndDate tl id date)
                        putStrLn "Enter new reservation details: "
                        date <- askForDayTimeValue

                        period <- askForTimePeriod
                        seats <- askForNumericValue "How many angry people: "

                        
                        let available_tables = tablesReadyToReserve date period seats new_tl
                        
                        if length available_tables > 0 then do
                                    putStrLn "Available tables: "
                                    putStrLn (showDB available_tables)
                                    id <- askForID available_tables False
                                    if id == 0 then return tl
                                        else return (addReservation new_tl id name date period)
                                        else do
                                                putStrLn "No available tables found"
                                                return tl
                                                
        else do
                putStrLn "No reservations found"
                return tl
    
    putStrLn ("\nReservation status for " ++ name ++ ":")
    putStrLn (showDB (filterTablesWithReservByName new_tl name))
    return new_tl

actReservDelByName :: TableList -> IO TableList
actReservDelByName tl = do
    putStrLn "\nReservation Delete"
    
    name <- askForStringValue "Angry client's surname: "
    
    let available_tables = filterTablesWithReservByName tl name
    new_tl <- if length available_tables > 0 then do
            putStrLn (showDB available_tables)
            
            putStrLn "\nMenu:"
            putStrLn "\t1 - All"
            putStrLn "\t2 - All at Date"
            putStrLn "\t3 - By Table ID & Date"
            putStr "Command? "; hFlush stdout
            q <- getLine
            tl <- case q of
                "1"     ->  return (remReservationByName tl name)
                "2"     ->  do date <- askForDayTimeValue;
                                return (remReservationByNameAndDate tl name date)
                "3"     ->  do id <- askForID available_tables False;
                                if id == 0 then return tl
                                    else do date <- askForDayTimeValue; return (if id /= 0 then (remReservationByIDAndDate tl id date) else tl)
            
            putStrLn ("\nReservation status for " ++ name ++ ":")
            putStrLn (showDB (filterTablesWithReservByName tl name))
            return tl
        else do
            putStrLn "No reservations found"
            return tl
    
    return new_tl



actSearchForFreeTablesByDate tl = do
    putStrLn "\nSearch for: Free table, by date"
    
    date <- askForDayTimeValue;
    
    tl <- return (map (\(Table id seats desc res) -> (id, seats, desc, (getReservationMaxPeriodAtDate res date))) tl)
    tl <- return (filter (\(_, _, _, period) -> period > (TimeDiff 0 0 0 0 0 0 0)) tl)
    
    let showSlots (id, seats, desc, period) = "ID: " ++ show id
            ++ "\tSeats: " ++ show seats
            ++ "\tFree Time: " ++ (if period == (TimeDiff 1 0 0 0 0 0 0) then "*:*" else showPeriod period)
            ++ "\tDesc: " ++ desc
            ++ "\n"
    
    putStrLn (concat (map (showSlots) tl))
    return ()

actSearchForReservByName tl = do
    putStrLn "\nSearch for: Reservations, by Name"
    
    name <- askForStringValue "Angry client's surname: "
    
    new_tl <- return (filterTablesWithReservByName tl name)
    if length new_tl > 0
        then putStrLn (showDB new_tl)
        else putStrLn "Client doesn't have any reservations";
    return ()

