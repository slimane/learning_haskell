data DateTuple = DateTuple {year :: Int, month :: Int, day :: Int} deriving(Show, Eq, Ord)







age :: (Int, Int, Int) -> (Int, Int, Int) -> Maybe Int
age (y1, m1, d1) (y2, m2, d2)
    | not $ validateDate date1  = Nothing
    | not $ validateDate date2  = Nothing
    | y1 > y2                   = Just $ calcAge date1 date2
    | y1 <= y2                  = Just $ calcAge date2 date1
    | otherwise                 = Nothing
    where date1 = DateTuple{year = y1, month = m1, day = d1}
          date2 = DateTuple{year = y2, month = m2, day = d2}
          temp_age = y1 - y2

calcAge :: DateTuple -> DateTuple -> Int
calcAge current_date birth_date
    | current_m > birth_m               = temp_age
    | current_m == birth_m
        && current_d >= birth_d         = temp_age
    | otherwise                         = temp_age - 1
    where current_m = month current_date
          current_d = day current_date
          birth_m = month birth_date
          birth_d = day birth_date
          temp_age = year current_date - year birth_date




-- date validation functions
validateDate :: DateTuple -> Bool
validateDate date =
    (validateMonth $ month date)
        && (validateDay date)


validateMonth :: Int -> Bool
validateMonth month
    | month < 1  = False
    | month > 12 = False
    | otherwise  = True


validateDay :: DateTuple -> Bool
validateDay date
    | d > 31                            = False
    | d == 31
        && not (m `elem` thirtyFirst)   = False
    | m == 2
        && d == 29
        && y `mod` 4 == 0               = True
    | m == 2 && d >= 29                 = False
    | d < 1                             = False
    | otherwise                         = True
    where d = day date
          m = month date
          y = year date
          thirtyFirst = [1, 3, 5, 7, 8, 10, 12]
