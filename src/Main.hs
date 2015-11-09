{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Time.Calendar as C
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

import Text.Mustache
import Data.Text (unpack)

import Debug.Trace

data QuadState = No | Start | Mid | End deriving (Eq, Show)

posRelTo :: (Ord a) => a -> (a, a) -> QuadState
posRelTo x (a, b)
  | x == a = Start
  | x == b = End
  | x > a && x < b = Mid
  | otherwise = No

data CalCell = CalCell
    Int -- ^ day of month (1..)
    Int -- ^ day of week (1..)
    QuadState -- ^ isLectureTime (QuadState)
    (Maybe String) -- ^ holiday text
data Month = Month Int [CalCell] -- ^ month (1..) and its entries

data RenderableMonth = RenderableMonth Int String [CalCell]

instance ToMustache RenderableMonth where
  toMustache (RenderableMonth i name cs) = object
    [ "xoffset" ~> toMustache (0.125 + 4.25 * fromIntegral i :: Float) -- 4-wide month column
    , "monthname" ~> name
    , "days" ~> map toMustache cs
    ]

instance ToMustache CalCell where
  toMustache (CalCell d wd lec mhol) = object $ holidayList ++
    [ "monthday" ~> d
    , "dayname" ~> (daysShort !! (wd - 1))
    , "isWeekday" ~> (wd < 6 && mhol == Nothing)
    , "isWeekend" ~> (wd > 5 || mhol /= Nothing) -- If this was real code, I'd write a big "Caution, assumption!" here
    , "isLecture" ~> (lec /= No)
    , "isLectureStart" ~> (lec == Start)
    , "isLectureEnd" ~> (lec == End)
    ]
    where holidayList = case mhol of
                          Nothing -> []
                          Just hol -> ["holiday" ~> hol]

daysLong, daysShort, months :: [String]
daysLong = words "Montag Dienstag Mittwoch Donnerstag Freitag Samstag Sonntag"
daysShort = map (take 2) daysLong

months = words "Januar Februar März April Mai Juni Juli August September Oktober November Dezember"

lecturePhases :: [(C.Day, C.Day)]
lecturePhases = map ((\((x1,x2,x3),(y1,y2,y3)) -> (C.fromGregorian x1 x2 x3, C.fromGregorian y1 y2 y3)))
  [ -- WS 15/16
    ((2015,10,12), (2015,12,19))
  , ((2016, 1, 4), (2016, 2, 6))
    -- SS 16
  , ((2016, 4, 4), (2016, 5,13))
  , ((2016, 5,23), (2016, 7,16))
    -- WS 16/17
  , ((2016,10,10), (2016,12,21))
  , ((2017,01,04), (2017,02,04))
    -- SS 17
  , ((2017, 4, 3), (2017, 6, 2))
  , ((2017, 6,12), (2017, 7,15))
  ]

holidays = map (\((y,m,d), s) -> (C.fromGregorian y m d, s))
  [ ((2015,10, 3), "Tag der Deutschen Einheit")
  , ((2015,10,31), "Reformationstag")
  , ((2015,11,18), "Buß- und Bettag")
  , ((2015,12,25), "1. Weihnachtstag")
  , ((2015,12,26), "2. Weihnachtstag")
  , ((2016, 1, 1), "Neujahr")
  , ((2016, 3,25), "Karfreitag")
  , ((2016, 3,28), "Ostermontag")
  , ((2016, 5, 1), "Tag der Arbeit")
  , ((2016, 5, 5), "Christi Himmelfahrt")
  , ((2016, 5,16), "Pfingstmontag")
  , ((2016,10, 3), "Tag der Deutschen Einheit")
  , ((2016,10,31), "Reformationstag")
  , ((2016,11,16), "Buß- und Bettag")
  , ((2016,12,25), "1. Weihnachtstag")
  , ((2016,12,26), "2. Weihnachtstag")
  , ((2017, 1, 1), "Neujahr")
  , ((2017, 4,14), "Karfreitag")
  , ((2017, 4,17), "Ostermontag")
  , ((2017, 5, 1), "Tag der Arbeit")
  , ((2017, 5,25), "Christi Himmelfahrt")
  , ((2017, 6, 5), "Pfingstmontag")
  ]

genCal
  :: Integer -- ^ start year
  -> Int -- ^ start month
  -> [Month]
genCal syear smonth = firstYear ++ secondYear
  where
    firstYear = map (genMonth syear) [smonth..12]
    secondYear = map (genMonth (syear + 1)) [1..smonth - 1]
    genMonth y m = Month m
                 $ map (genCell . C.fromGregorian y m)
                 $ [1..(monthLength (C.isLeapYear y) m)]
    genCell day = let (_, _, weekday) = toWeekDate day
                      (_, _, monthday) = C.toGregorian day
                  in CalCell monthday weekday (lookupLecture day) (lookup day holidays)
    lookupLecture day = case filter (/= No) $ map (posRelTo day) lecturePhases of
                          [] -> No
                          [s] -> s

{-
renderCalCell :: CalCell -> RenderableCalCell
renderCalCell (CalCell d wd l)
  | wd < 6    = RenderableCalCell d "#eeeeee" lcolor
  | otherwise = RenderableCalCell d "#aaffaa" lcolor
  where lcolor = case l of
                   0 -> ""
                   1 -> ""
-}

renderMonth :: (Month, Int) -> RenderableMonth
renderMonth (Month m cs, i) = RenderableMonth i (months !! (m-1)) cs

main = do
  compiled <- automaticCompile ["."] "template.mustache"
  template <- case compiled of
    Left err -> error $ show err
    Right template -> return template
  
  let cal = genCal 2015 10
      renderableCal = map renderMonth $ zip cal [0..]
  
  writeFile "kalender.svg" $ unpack $ substitute template $ renderableCal
