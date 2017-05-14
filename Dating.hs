
module Dating where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale


prettify :: Day -> String
prettify day = date ++ (ordinal date) ++ " " ++ month
  where month = formatTime defaultTimeLocale "%b" day
        date = formatTime defaultTimeLocale "%-d" day


ordinal :: String -> String
ordinal "1" = "st"
ordinal "2" = "nd"
ordinal "3" = "rd"
ordinal "21" = "st"
ordinal "22" = "nd"
ordinal "23" = "rd"
ordinal "31" = "st"
ordinal _ = "th"


foreverFrom :: Day -> [Day]
foreverFrom day = day:(foreverFrom (addDays 1 day))


isASunday :: Day -> Bool
isASunday day = weekday == "0"
  where weekday = formatTime defaultTimeLocale "%w" day


nextFifteenSundays :: IO [String]
--nextFifteenSundays = ["1st Jan", "2nd Jan"]
nextFifteenSundays = do
    now <- getCurrentTime
    return [prettify d | d <- take 15 $ filter isASunday (foreverFrom (utctDay now))]
