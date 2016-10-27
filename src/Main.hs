{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Time.Calendar as C
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Control.Arrow (first, (***))

import Text.Mustache
import Text.Mustache.Compile
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
    QuadState -- ^ isLectureTime
    QuadState -- ^ isExamTime
    Bool -- ^ isFullHoliday
    (Maybe String) -- ^ text
data Month = Month Int [CalCell] -- ^ month (1..) and its entries

data RenderableMonth = RenderableMonth Int String [CalCell]

instance ToMustache RenderableMonth where
  toMustache (RenderableMonth i name cs) = object
    [ "xoffset" ~> toMustache (0.125 + 4.25 * fromIntegral i :: Float) -- 4-wide month column
    , "monthname" ~> name
    , "days" ~> map toMustache cs
    ]

instance ToMustache CalCell where
  toMustache (CalCell d wd lec exam fullhol mtext) = object $
    [ "monthday" ~> d
    , "dayname" ~> (daysShort !! (wd - 1))
    , "isWeekendOrHoliday" ~> (wd > 5 || fullhol)
    , "isLecture"   ~> (lec /= No)
    , "isNoLecture" ~> (lec == No)
    , "isLectureStart" ~> (lec == Start)
    , "isLectureEnd"   ~> (lec == End)
    , "isExam"   ~> (exam /= No)
    , "isNoExam" ~> (exam == No)
    ] ++ case mtext of
           Nothing -> []
           Just t -> ["text" ~> t]

daysLong, daysShort, months :: [String]
daysLong = words "Montag Dienstag Mittwoch Donnerstag Freitag Samstag Sonntag"
daysShort = map (take 2) daysLong

months = words "Januar Februar März April Mai Juni Juli August September Oktober November Dezember"

tupleToDate (y,m,d) = C.fromGregorian y m d

lecturePhases :: [(C.Day, C.Day)]
lecturePhases = map (tupleToDate *** tupleToDate)
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
    -- WS 17/18
  , ((2017,10,09), (2017,12,20))
  , ((2018,01,04), (2018,02,03))
    -- SS 18
  , ((2018, 4, 9), (2018, 5,18))
  , ((2018, 5,28), (2018, 7,21))
  ]

examPhases :: [(C.Day, C.Day)]
examPhases = map (tupleToDate *** tupleToDate)
  [ -- WS 17/18
    ((2017, 2, 6), (2017, 3, 4))
    -- SS 17
  , ((2017, 7,17), (2017, 8,12))
    -- WS 17/18
  , ((2018, 2, 5), (2018, 3, 3))
    -- SS 18
  , ((2018, 7,23), (2018, 8,18))
  ]

holidaysFromList = map (first tupleToDate) 

fullHolidays = holidaysFromList
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
  , ((2017,10, 3), "Tag der Deutschen Einheit")
  , ((2017,10,31), "Reformationstag")
  , ((2017,11,22), "Buß- und Bettag")
  , ((2017,12,25), "1. Weihnachtstag")
  , ((2017,12,26), "2. Weihnachtstag")
  ]
uniHolidays = holidaysFromList
  [ ((2016, 6, 1), "Dies academicus")
  , ((2017, 5,17), "Dies academicus")
  ]
noHolidays = holidaysFromList
  [ ((2017, 6,15), "OUTPUT")
  , ((2017, 6,16), "LNdW")
  ]


genCal
  :: Integer -- ^ start year
  -> Int -- ^ start month
  -> Int -- ^ additional months
  -> [Month]
genCal syear smonth addmonths = firstYear ++ secondYear
  where
    firstYear = map (genMonth syear) [smonth..12]
    secondYear = map (genMonth (syear + 1)) [1..(smonth - 1 + addmonths)]
    genMonth y m = Month m
                 $ map (genCell . C.fromGregorian y m)
                 $ [1..(monthLength (C.isLeapYear y) m)]
    genCell day
      = let (_, _, weekday) = toWeekDate day
            (_, _, monthday) = C.toGregorian day
            isInLecturePhase
              | lookup day uniHolidays /= Nothing = No
              | otherwise = case filter (/= No) $ map (posRelTo day) lecturePhases of
                              [s] -> s
                              [] -> No
            isInExamPhase
              = case filter (/= No) $ map (posRelTo day) examPhases of
                  [s] -> s
                  [] -> No
            isFullHoliday = lookup day fullHolidays /= Nothing
            text = lookup day $ fullHolidays ++ uniHolidays ++ noHolidays
        in CalCell monthday weekday isInLecturePhase isInExamPhase isFullHoliday text

renderMonth
  :: (Month, Int) -- ^ month and its zero-based position in calendar
  -> RenderableMonth
renderMonth (Month m cs, i) = RenderableMonth i (months !! (m-1)) cs

tpl = $(embedTemplate ["."] "template.mustache")

main = do
  let cal = genCal 2017 1 0
      renderableCal = zipWith (curry renderMonth) cal [0..]
  
  writeFile "generated.svg" $ unpack $ substitute tpl renderableCal
