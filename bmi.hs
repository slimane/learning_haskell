module Bmi where
getBmi :: Float -> Float -> Float
getBmi height weight = weight / ( height / 100 )**2
getWeight :: Float -> Float -> Float
getWeight height bmi = bmi * (height / 100)**2
getWeightBmi :: Float -> Float -> (Float, Float)
getWeightBmi height bmi = ( bmi, getWeight height bmi )
getBmiCat :: Float -> Float -> String
getBmiCat height weight
    | bmi == medicalIdeal      = "medical ideal"
    | bmi >= modelMin
            && bmi <= modelMax = "model fit"
    | bmi >= americanFat       = "american fat"
    | bmi >= americanStandard  = "american standard"
    | bmi >= fat               = "fat"
    | bmi >= standard          = "standard"
    | bmi >= low               = "low"
    | low >= bmi               = error "height and weight must be positive"
    where
        bmi = weight / (height / 100)**2
        medicalIdeal     = 22
        modelMax         = 19.5
        modelMin         = 18.5
        low              = 0
        standard         = 18.5
        fat              = 25.0
        americanStandard = 30.0
        americanFat      = 40.0


getBmiCatX height weight =
    let
        calcBmi height weight = weight / (height/100)**2
    in
        calcBmi height weight




-- utility
minutesToHour :: Float -> Float
minutesToHour minutes = minutes / 60

month :: Float
month = 365.25 / 12




data Activity = Activity { kind :: String, speed :: Float} deriving(Show, Ord, Eq)

--  calculate calorie
getCalorie :: (Float , String, Float) -> Float
getCalorie (weight, activity, time) =
    weight * 1.05 * minutesToHour( time * getMets(activity, time) )





-- calculate mets
getMets :: ( String, Float ) -> Float
getMets ( activity, speed )
    | activity == "Run"  = ( getMetsRun   speed)
    | activity == "Walk" = ( getMetsWalk  speed)
    | otherwise          = error "argument is unmatching"


getMetsRun :: Float -> Float
getMetsRun speed
    | 6 <= speed && speed < 8 = speed * 0.7
    | 8 <= speed              = speed
    | otherwise               = 0


getMetsWalk :: Float -> Float
getMetsWalk speed
    | 1 <= speed && speed < 3 = speed * 0.3
    | 3 <= speed && speed < 5 = speed * 0.5
    | 5 <= speed && speed < 6 = speed * 0.6
    | 6 <= speed && speed < 7 = speed * 0.75
    | 7 <= speed && speed < 8 = speed * 0.9
    | 8 <= speed              = speed
    | otherwise               = 0


getMetsOther :: Float -> Float
getMetsOther speed = 0



(!) :: Integer -> Integer
(!) n
    | n == 0    = 1
    | n > 0     = n * (!) (n - 1)
    | otherwise = error "this function need positive Integer"



getHarrisBenedictEquation :: String -> Float -> Float -> Float -> Float
getHarrisBenedictEquation gender age height weight
    | gender == "m"  = 66.5  + (weight * 13.8)  + (height * 5.0) - (age * 6.8)
    | gender == "jm" = 66.47 + (weight * 13.75) + (height * 5.0) - (age * 6.76)
    | gender == "f"  = 655   + (weight * 9.6)   + (height * 1.8) - (age * 4.7)
    | otherwise      = error "gender must be \"m\"(male), \"f\"(female) or \"jm\"(japanese male)"

