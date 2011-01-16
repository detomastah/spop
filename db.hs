module DB where

import Debug.Trace
import IO
import System.Time
import Char

{- nazwisko, data, okres, uwagi -}
data Reservation = Reservation String CalendarTime TimeDiff String deriving(Show, Read)

{- numer stolika, ilosc siedzen, opis, rezerwacje -}
data Table = Table Int Int String [Reservation] deriving(Show, Read)

table :: Int -> Int -> String -> Table
table i seats desc = Table i seats desc []

type TableList = [Table]

calGetYear (CalendarTime year month day hour min _ _ _ _ _ _ _) = year
calGetMonth (CalendarTime year month day hour min _ _ _ _ _ _ _) = fromEnum month
calGetDay (CalendarTime year month day hour min _ _ _ _ _ _ _) = day

addTable :: Table -> TableList -> TableList
addTable t [] = [t]
addTable t tl = t:tl

remById :: Int -> TableList -> TableList
remById ii [] = []
remById ii ((Table id seats desc res):xs) = if ii == id then xs else (Table id seats desc res):(remById ii xs)

getSeats (Table _ seats _ _) = seats
getReservations (Table _ _ _ reservations) = reservations
getID (Table id _ _ _) = id

getName (Reservation name date period desc) = name
getDate (Reservation name date period desc) = date

validateExistenceOfTable :: TableList -> Int -> Bool
validateExistenceOfTable tl id = 0 == length (filter ((== id).getID) tl)

validateNumericalityOf str = foldr (&&) True (map isDigit str) 

findTableByID (t:ts) id = 
    if (getID t) == id then t else findTableByID ts id

showDate (CalendarTime ctYear ctMonth ctDay ctHour ctMin ctSec ctPicosec ctWDay ctYDay ctTZName ctTZ ctIsDST) =
    (show ctMonth) ++ " " ++ (show ctDay) ++ " " ++ (show ctHour) ++ ":" ++ (show ctMin)
showPeriod (TimeDiff tdYear tdMonth tdDay tdHour tdMin tdSec tdPicosec) = 
    (show tdHour) ++ ":" ++ (show tdMin)

showReserv :: [Reservation] -> String
showReserv [] = "";
showReserv ((Reservation name date period desc):xs) = ("\tName: " ++ name ++ "\tDate: " ++ (showDate date) ++ "\tPeriod: " ++ (showPeriod period) ++ "\tDesc: " ++ desc ++ "\n") ++ (showReserv xs);

showTable :: Table -> String
showTable (Table i seats desc xs) = "ID: " ++ show i ++ "\tSeats: " ++ show seats ++ "\tDesc: " ++ desc ++ "\n" ++ (showReserv xs);

showDB :: TableList -> String
showDB [] = "Empty"
showDB [x] = (showTable x)
showDB (x:xs) = (showTable x) ++ (showDB xs)

saveDB :: TableList -> FilePath -> IO ()
saveDB tl path = do
    h <- openFile path WriteMode
    hPutStr h (show tl)
    hClose h
    return ()

loadDB :: FilePath -> IO TableList
loadDB path = do
    h <- openFile path ReadMode
    cont <- hGetContents h
    return $! (read cont)


getTimeDifference ct1 ct2 = normalizeTimeDiff (diffClockTimes (toClockTime ct1) (toClockTime ct2))

defaultPeriod = TimeDiff 0 0 0 2 0 0 0
minutesPeriod i = (TimeDiff 0 0 0 0 i 0 0)


-- CHECK IF IT IS VALID!!!
testTableReservationAbility [] date period = True
testTableReservationAbility ((Reservation _ date period _):xs) ndate nperiod =
    if ndate >= date then
            if getTimeDifference ndate date >= period then True && testTableReservationAbility xs ndate nperiod else False
        else
            if getTimeDifference date ndate >= nperiod then True && testTableReservationAbility xs ndate nperiod else False

findTablesWithSufficientSeats seats tl = filter (\t -> (getSeats t) >= seats) tl

findFreeTablesByDateAndTime date period tl = filter (\t -> testTableReservationAbility (getReservations t) date period) tl

tablesReadyToReserve date period seats tl = findFreeTablesByDateAndTime date period (findTablesWithSufficientSeats seats tl)

-- STUB
addReservationToTable (Table id seats desc reservations) name date period = Table id seats desc new_reservations
    where new_reservations = (Reservation name date period ""):reservations
addReservation tl id name date period = (addReservationToTable (findTableByID tl id) name date period):(remById id tl)


remReservationFromTableByName_ (Table id seats desc res) name = (Table id seats desc (filter (\x -> (getName x) /= name) res))
remReservationFromTableByDate_ (Table id seats desc res) date = (Table id seats desc (filter (\x -> (getDate x) /= date) res))
remReservationFromTableByNameAndDate_ (Table id seats desc res) name date = (Table id seats desc (filter (\x -> ((getName x /= name) || (getDate x /= date))) res))

remReservationByName tl name = map (\x -> remReservationFromTableByName_ x name) tl
remReservationByNameAndDate tl name date = map (\x -> remReservationFromTableByNameAndDate_ x name date) tl
remReservationByIDAndDate tl id date = (remReservationFromTableByDate_ (findTableByID tl id) date):(remById id tl)

filterTablesWithReservByName [] name = []
filterTablesWithReservByName ((Table id seats desc res):ts) name =
    let onlyname = (filter (\x -> (getName x) == name) res)
    in if length onlyname == 0
        then filterTablesWithReservByName ts name
        else (Table id seats desc onlyname):(filterTablesWithReservByName ts name)

{-

tl <- loadDB "database"
putStrLn (showDB tl)
date <- askForDayTimeValue
putStrLn (showDB (remReservationByNameAndDate tl "Bla2" date))

   -}
