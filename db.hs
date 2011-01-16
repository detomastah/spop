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

validateExistenceOfTable :: TableList -> Int -> Bool
validateExistenceOfTable tl id = 0 == length (filter ((== id).getID) tl)

validateNumericalityOf str = foldr (&&) True (map isDigit str) 

tlgetTableID (t:ts) id = 
    if (getID t) == id then t else tlgetTableID ts id

showReserv :: [Reservation] -> String
showReserv [] = "";
showReserv ((Reservation name cal diff desc):xs) = ("\tName: " ++ name ++ " Cal: " ++ show cal ++ " Time: " ++ show diff ++ " Desc: " ++ desc ++ "\n") ++ (showReserv xs);

showTable :: Table -> String
showTable (Table i seats desc xs) = "ID: " ++ show i ++ " Seats: " ++ show seats ++ " Desc: " ++ desc ++ "\n" ++ (showReserv xs);

showDB :: TableList -> String
showDB [] = "Empty"
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


getTimeDifference ct1 ct2 = diffClockTimes (toClockTime ct1) (toClockTime ct2)

defaultPeriod = TimeDiff 0 0 0 2 0 0 0


-- CHECK IF IT IS VALID!!!
testTableReservationAbility [] cal diff = True
testTableReservationAbility ((Reservation _ cal diff _):xs) ncal ndiff =
    if ncal >= cal then
            if normalizeTimeDiff (getTimeDifference ncal cal) >= diff then True && testTableReservationAbility xs ncal ndiff else False
        else
            if normalizeTimeDiff (getTimeDifference cal ncal) >= ndiff then True && testTableReservationAbility xs ncal ndiff else False

findTablesWithSufficientSeats seats tl = filter (\t -> (getSeats t) >= seats) tl

findFreeTablesByDateAndTime date time tl = filter (\t -> testTableReservationAbility (getReservations t) date time) tl

tablesReadyToReserve date seats tl = findFreeTablesByDateAndTime date defaultPeriod (findTablesWithSufficientSeats seats tl)

-- STUB
addReservationToTable_ (Table id seats desc reservations) name day = Table id seats desc ((Reservation name day defaultPeriod ""):reservations)
addReservationToTable tl id name day  = (addReservationToTable_ (tlgetTableID tl id) name day):(remById id tl)

