-- A1
-- a, b
ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- c, d
ackermann' :: (Integral a) => a -> a -> a
ackermann' m n | m == 0 = n + 1
               | n == 0 = ackermann' (m - 1) 1
               | otherwise = ackermann' (m - 1) (ackermann' m (n - 1))
-- e
ackermann'' m n = case (m, n) of
    (0, _) -> n + 1
    (_, 0) -> ackermann'' (m - 1) 1
    (_, _) -> ackermann'' (m - 1) (ackermann'' m (n - 1))


-- A2
-- a
isLeapYear :: Int -> Bool
isLeapYear n = (n `mod` 400 == 0) || ((n `mod` 4 == 0) && (n `mod` 100 /= 0))

-- b
data Month =
      Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Okt
    | Nov
    | Dec
    deriving (Enum, Eq, Show)

type Day = Int
type Year = Int

-- c
data Date = Date Day Month Year
    deriving (Show)

-- d
daysInMonth :: Month -> Year -> Int
daysInMonth Feb year
    | isLeapYear year = 29
    | otherwise       = 28
daysInMonth Apr _ = 30
daysInMonth Jun _ = 30
daysInMonth Sep _ = 30
daysInMonth Nov _ = 30
daysInMonth _ _   = 31

dateIsValid :: Date -> Bool
dateIsValid (Date day month year) = (year > 0 && year < 9999) && (day > 0 && day < (daysInMonth month year))

-- e
dayOfYear :: Date -> Int
dayOfYear (Date day month year)
    | month == Jan = day
    | otherwise    = day + sum (map (`daysInMonth` year) [Jan .. pred month])

daysTillYear :: Year -> Int
daysTillYear year = sum (map daysInYear [0 .. year - 1])

daysInYear :: Year -> Int
daysInYear year
    | isLeapYear year = 366
    | otherwise       = 365

totalDaysFromStart :: Date -> Int
totalDaysFromStart (Date day month year) = dayOfYear (Date day month year) + daysTillYear year

daysBetween :: Date -> Date -> Int
daysBetween (Date day1 month1 year1) (Date day2 month2 year2)
    | year1 == year2 = abs (dayOfYear (Date day1 month1 year1) - dayOfYear (Date day2 month2 year2))
    | otherwise      = abs (totalDaysFromStart (Date day1 month1 year1) - totalDaysFromStart (Date day2 month2 year2))

-- f
data Weekday =
      Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
    deriving (Enum, Eq, Show)

-- g
referenceDate :: Date
referenceDate = Date 3 Jan 2025

weekday :: Date -> Weekday
weekday date = toEnum $ (((totalDaysFromStart date - totalDaysFromStart referenceDate) `mod` 7) + fromEnum Fri) `mod` 7
