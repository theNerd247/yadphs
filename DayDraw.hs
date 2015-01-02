{-# LANGUAGE OverloadedStrings #-}
module DayDraw 
(
lineAt
,formatDesc
)
where
import Data.List
import Data.Text (count,pack)
import Control.Applicative

vcolChar = '|'
rowChar = '-'
colWidth = 20
minutesPerLine = 30 
tMax = div (60*24+59) minutesPerLine

dayLine :: String -> String
dayLine s = 
	showString (vcolChar:"")
	$ showString s 
	$ showString (vcolChar:"")
	$ "\n"

blankLine :: String
blankLine = dayLine (replicate (colWidth-2) ' ')

nblanklines n = (concat $ replicate n blankLine)

line :: String
line = dayLine (replicate (colWidth-2) rowChar)

lineAt :: Integer -> String 
lineAt (-1) = ""
lineAt ns = 
	showString (nblanklines n) line
	where n = fromInteger ns

taskLine :: Priority -> Time -> Integer -> String
taskLine p t ns = 
	showString (nblanklines n)
	$ dayLine
	$ showString (rowChar:"")
	$ showString (show p)
	$ showString (replicate (colWidth-12) rowChar)
	$ showString (show t)
	$ rowChar:""
	where n = fromInteger ns

{-printDesc :: String -> String-}
{-printDesc [] = ""-}
{-printDesc d -}
	{-| l < ll = dayLine $ d ++ (replicate (ll-l) ' ')-}
	{-| otherwise = (dayLine $ fst spn) ++ (printDesc $ snd spn)-}
	{-where -}
		{-l = length d-}
		{-ll = colWidth-2-}
		{-spn = splitAt ll d-}

{-makeEventText :: (String,Event) -> EventText-}
{-makeEventText (d,e) = EventText st et ds-}
	{-where -}
		{-st = timeHeight (time . startDate $ e)-}
		{-dt = nlines ds-}
		{-ds = printDesc d-}
		{-et = timeHeight (time . endDate $ e) - st - dt-}

{-nlines = count "\n" . pack-}

{--- converts a time into the proper y dimensions for the graph-}
{-timeHeight :: Time -> Int-}
{-timeHeight Time {hour = h, minute = m} = div (60*h+m) minutesPerLine-}

{--- convert events into a list of days-}
{-printDay :: [(String,Event)] -> String-}
{-printDay es = (show $ norm $ a)-}
	{-++ (concat $ replicate (tMax - (maxi a)) blankLine)-}
	{-where a = makeEventText <$> es-}

-- print a list of days (each day is a list of events) in weekly format
{-printAsWeeks :: [[(String,Event)]] -> String-}
{-printAsWeeks = formatWeek . (printDay <$>)-}

{-formatWeek :: [String] -> String-}
{-formatWeek [] = ""-}
{-formatWeek ds = (comDays $ fst dds) -}
	{-++ "\n" -}
	{-++ formatWeek (snd dds)-}
	{-where	-}
		{-comDays = unlines . zipDays . (lines <$>)-}
		{-dds = splitAt 7 ds-}

{-zipDays :: [[String]] -> [String]-}
{-zipDays [c] = c-}
{-zipDays (d:ds) = zipWith (++) d (zipDays ds)-}
