module DayDraw 
(
printDay
)
where
import Task
import Data.List
import Control.Applicative

vcolChar = '|'
rowChar = '-'
colWidth = 20
minutesPerLine = 15 

type DayBlock = [Int]

dayLine :: String -> String
dayLine s = 
	showString "|" 
	$ showString s 
	$ showString "|"
	$ "\n"

blankLine :: String
blankLine = dayLine (replicate (colWidth-2) ' ')

line :: String
line = dayLine (replicate (colWidth-2) '-')

lineAtHeight :: Int -> String 
lineAtHeight (-1) = ""
lineAtHeight n = 
	showString (concat $ replicate n blankLine) line

showDayBlock :: [Int] -> String
showDayBlock [] = ""
showDayBlock [a] = ""
showDayBlock (nb:lst@(n:ns)) = 
	showString (lineAtHeight (n-nb-1))
	$ showDayBlock lst

-- wrapper around showDayBlock this is the safe function to use
-- do not directly call showDayBlock
showDay :: [Int] -> String
showDay ns = showDayBlock (0:ns) 
	++ (concat $ replicate (24-(maximum ns)) blankLine)

-- converts a time into the proper y dimensions for the graph
timeHeight :: Time -> Int
timeHeight Time {hour = h, minute = m} = div (60*h+m) minutesPerLine

eventHeight :: Event -> [Int]
eventHeight Event {startDate = sd, endDate = ed} = [timeHeight $ (time sd), timeHeight $ (time ed)]

printDay :: [Event] -> String
printDay  = showDay . sort . concat . (eventHeight <$>)

formatWeek :: [String] -> String
formatWeek [] = ""
formatWeek ds = (comDays $ fst dds) 
	++ "\n" 
	++ formatWeek (snd dds)
	where	
		comDays = unlines . zipDays . (lines <$>)
		dds = splitAt 7 ds

zipDays :: [[String]] -> [String]
zipDays [c] = c
zipDays (d:ds) = zipWith (++) d (zipDays ds)