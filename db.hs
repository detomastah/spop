module DB where

import Debug.Trace
import IO
import System.Time
import Char

quicksort  []           =  []
quicksort (x:xs)        =  quicksort [y | y <- xs, y<x ]
                        ++ [x]
                        ++ quicksort [y | y <- xs, y>=x]

{- nazwisko, data, okres, uwagi -}
data Reservation = Reservation String CalendarTime TimeDiff String deriving(Show, Read)

instance Eq Reservation where
    (Reservation name0 date0 period0 desc0) == (Reservation name1 date1 period1 desc1) =
        name0 == name1 && date0 == date1 && period0 == period1 && desc0 == desc1

instance Ord Reservation where
    (Reservation name0 date0 period0 desc0) < (Reservation name1 date1 period1 desc1) =
        date0 < date1
    (Reservation name0 date0 period0 desc0) > (Reservation name1 date1 period1 desc1) =
        date0 > date1
    (Reservation name0 date0 period0 desc0) <= (Reservation name1 date1 period1 desc1) =
        date0 <= date1
    (Reservation name0 date0 period0 desc0) >= (Reservation name1 date1 period1 desc1) =
        date0 >= date1


{- numer stolika, ilosc siedzen, opis, rezerwacje -}
data Table = Table Int Int String [Reservation] deriving(Show, Read)

instance Eq Table where
    (Table id0 seats0 desc0 res0) == (Table id1 seats1 desc1 res1) =
        id0 == id1 && seats0 == seats1 && desc0 == desc1 && res0 == res1

instance Ord Table where
    (Table id0 _ _ _) < (Table id1 _ _ _) = id0 < id1
    (Table id0 _ _ _) > (Table id1 _ _ _) = id0 > id1
    (Table id0 _ _ _) <= (Table id1 _ _ _) = id0 <= id1
    (Table id0 _ _ _) >= (Table id1 _ _ _) = id0 >= id1
        
table :: Int -> Int -> String -> Table
table i seats desc = Table i seats desc []

type TableList = [Table]

calGetYear (CalendarTime year month day hour min _ _ _ _ _ _ _) = year
calGetMonth (CalendarTime year month day hour min _ _ _ _ _ _ _) = month
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


showTable (Table i seats desc xs) = "ID: " ++ show i ++ "\tSeats: " ++ show seats ++ "\tDesc: " ++ desc ++ "\n" ++ (showReserv xs);
showJustTable (Table i seats desc xs) = "ID: " ++ show i ++ "\tSeats: " ++ show seats ++ "\tDesc: " ++ desc ++ "\n"


showDB [] = "Empty"
showDB [x] = (showTable x)
showDB (x:xs) = (showTable x) ++ (showDB xs)

showJustTables [] = "Empty"
showJustTables tl = concat (map (showJustTable) tl)

saveDB :: TableList -> FilePath -> IO ()
saveDB tl path = do
    h <- openFile path WriteMode
    hPutStr h (show tl)
    hClose h
    return ()

loadDB_ :: FilePath -> IO TableList
loadDB_ path = do
    h <- openFile path ReadMode
    cont <- hGetContents h
    return $! (read cont)

loadDB path = catch (loadDB_ path) (\e -> do return [])

getTimeDifference ct1 ct2 = normalizeTimeDiff (diffClockTimes (toClockTime ct1) (toClockTime ct2))

minutesPeriod i = (TimeDiff 0 0 0 0 i 0 0)

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
addReservationToTable (Table id seats desc reservations) name date period = Table id seats desc (quicksort new_reservations)
    where new_reservations = (Reservation name date period ""):reservations
addReservation tl id name date period = (addReservationToTable (findTableByID tl id) name date period):(remById id tl)

remReservationFromTableByName (Table id seats desc res) name = (Table id seats desc (filter (\x -> (getName x) /= name) res))
remReservationFromTableByDate (Table id seats desc res) date = (Table id seats desc (filter (\x -> (getDate x) /= date) res))
remReservationFromTableByNameAndDate (Table id seats desc res) name date = (Table id seats desc (filter (\x -> ((getName x /= name) || (getDate x /= date))) res))

remReservationByName tl name = map (\x -> remReservationFromTableByName x name) tl
remReservationByNameAndDate tl name date = map (\x -> remReservationFromTableByNameAndDate x name date) tl
remReservationByIDAndDate tl id date = (remReservationFromTableByDate (findTableByID tl id) date):(remById id tl)

remReservationFromTableBeforeDate (Table id seats desc res) date = (Table id seats desc (filter (\x -> (getDate x) >= date) res))
remReservationBeforeDate tl date = map (\x -> remReservationFromTableBeforeDate x date) tl


filterTablesWithReservByName [] name = []
filterTablesWithReservByName ((Table id seats desc res):ts) name =
    let onlyname = (filter (\x -> (getName x) == name) res)
    in if length onlyname == 0
        then filterTablesWithReservByName ts name
        else (Table id seats desc onlyname):(filterTablesWithReservByName ts name)


existsReservationByDate [] date = False
existsReservationByDate (r:rs) date = if (getDate r) == date then True else existsReservationByDate rs date

getReservationMaxPeriodAtDate [] date = (TimeDiff 0 0 1 0 0 0 0)
getReservationMaxPeriodAtDate ((Reservation _ date period _):xs) ndate =
    if ndate >= date then
            if getTimeDifference ndate date >= period then getReservationMaxPeriodAtDate xs ndate else (TimeDiff 0 0 0 0 0 0 0)
        else
            getTimeDifference date ndate
