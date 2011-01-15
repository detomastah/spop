module DB where

import Debug.Trace
import IO
import System.Time
import Char

{- id stolika, nazwisko, data, czas, uwagi -}
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

remID :: Int -> TableList -> TableList
remID ii [] = []
remID ii ((Table i seats desc res):xs) = if ii == i then xs else (Table i seats desc res):(remID ii xs)

getSeats :: Table -> Int
getSeats (Table _ seats _ _) = seats

getID :: Table -> Int
getID (Table id _ _ _) = id

tableReservFits [] cal diff = True
tableReservFits ((Reservation _ cal diff _):xs) ncal ndiff =
    if ncal >= cal then
            if normalizeTimeDiff (diffClockTimes (toClockTime ncal) (toClockTime cal)) >= diff then True && tableReservFits xs ncal ndiff else False
        else
            if normalizeTimeDiff (diffClockTimes (toClockTime cal) (toClockTime ncal)) >= ndiff then True && tableReservFits xs ncal ndiff else False


searchFreeTables_MinSeats seats tl = filter (\t -> (getSeats t) >= seats) tl

searchFreeTables_Date_Time date time [] = []
searchFreeTables_Date_Time date time ((Table id seats desc res):xs) =
    if tableReservFits res date time then
        (Table id seats desc res):(searchFreeTables_Date_Time date time xs)
        else (searchFreeTables_Date_Time date time xs)
    


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
    return $! (read cont);

