{-# LANGUAGE OverloadedStrings #-}
module DayDraw 
(
printDay
,printAsWeeks
)
where
import Task
import Data.List
import Data.Text (count,pack)
import Control.Applicative

data Yadp = Yadp (String,Event)

dscr (Yadp (s,_)) = s
event (Yadp (_,e)) = e
toTup (Yadp a) = a

instance Eq Yadp where
	(==) (Yadp (_,a)) (Yadp (s,b)) = a == b

instance Ord Yadp where
	compare (Yadp (_,a)) (Yadp (_,b)) = compare a b

vcolChar = '|'
rowChar = '-'
colWidth = 20
minutesPerLine = 30 
tMax = div (60*24+59) minutesPerLine

data EventText = EventText
	{stTime :: Int -- starting height of the day block
	, endTime :: Int -- ending height of the day block
	, desc :: String -- this is the formatted version of an Event Description
	}

instance Show EventText where	
	show e = 
		showString (lineAtHeight $ stTime e)
		$ showString (desc e)
		$ lineAtHeight (endTime e)
	showList = showString . foldl (\x y -> x ++ (show y)) ""

type DayText = [EventText]

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

printDesc :: String -> String
printDesc [] = ""
printDesc d 
	| l < ll = dayLine $ d ++ (replicate (ll-l) ' ')
	| otherwise = (dayLine $ fst spn) ++ (printDesc $ snd spn)
	where 
		l = length d
		ll = colWidth-2
		spn = splitAt ll d

makeEventText :: (String,Event) -> EventText
makeEventText (d,e) = EventText st et ds
	where 
		st = timeHeight (time . startDate $ e)
		dt = nlines ds
		ds = printDesc d
		et = timeHeight (time . endDate $ e) - st - dt

nlines = count "\n" . pack

-- converts a time into the proper y dimensions for the graph
timeHeight :: Time -> Int
timeHeight Time {hour = h, minute = m} = div (60*h+m) minutesPerLine

-- convert events into a list of days
printDay :: [(String,Event)] -> String
printDay es = (show $ norm $ a)
	++ (concat $ replicate (tMax - (maxi a)) blankLine)
	where a = makeEventText <$> es

maxi :: [EventText] -> Int
maxi = endTime . max

norm e = normalize $ EventText 0 0 "" : e

normalize :: [EventText] -> [EventText]
normalize [e] = []
normalize (eb:lst@(e:es)) = (e `sub` eb) : normalize lst

sub :: EventText -> EventText -> EventText
sub (EventText {stTime = sta,endTime = et,desc = d}) (EventText {stTime = stb}) = EventText (sta - stb-1) et d

-- print a list of days (each day is a list of events) in weekly format
printAsWeeks :: [[(String,Event)]] -> String
printAsWeeks = formatWeek . (printDay <$>)

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
