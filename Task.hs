module Task 
(
allEvents
,filterEvents
,eventDate
,task
,Time(Time,hour,minute)
,EventDate(EventDate,date,time)
,EventFreq(Once,Every)
,Event(Event,startDate,endDate)
) where

import Control.Applicative
import Data.List 
import Data.Text
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

-- printing config data
vcolChar = '|'
rowChar = '-'
colWidth = 20
minutesPerLine = 30 
tMax = div (60*24+59) minutesPerLine

-- time structure
data Time = Time { hour :: Int 
                 , minute :: Int
								 } deriving (Eq,Ord)

instance Show Time where
	show (Time h m) = showString (show h) $ showString ":" $ (show m)

-- date and time combined into a useable form
data EventDate = NullDate | EventDate 
	{date :: Day
	,time :: Time
	} deriving (Eq,Ord)

-- convert an event date to a time height
eventHeight :: EventDate -> Integer
eventHeight (EventDate t d) =  ld + (toInteger lt)
	where 
		ld = div (60*24*ds) minutesPerLine
		ds = diffDays d (fromGregorian 0 0 0)
		lt = div (60*h+m) minutesPerLine
		h = hour t
		m = minute t

-- events can occur at different frequencies 
-- an event can occure once, or every n days on a list of days (1 being monday,
-- 7 being sunday) (the second parameter is optional)
data EventFreq = Once | Every {nDays :: Integer, onDays :: [Int]} deriving (Eq,Show,Ord)

-- custom type to handle task info
-- an event is simply the combination of a start and end date, and a frequency
-- at which the even occurs
data Event = Event 
	{eventDesc :: Text
	,eventStart :: EventDate
	,eventEnd :: EventDate
	} deriving (Ord)

instance Eq Event where
	(==) a b = (&&) 
		$ (eventStart a) == (eventStart b) 
		$ (eventEnd a) == (eventEnd b)

instance Num Eevent where
	(negate) (Event d es en) = Event d (-es) (-en)
	(+) (Event d es1 en1) (Event es2 en2) = Event d (es1+es2) (en1+en2)
 
instance Show Event where
	show = printEvent
	showList es = printEvents $ (Event "" NullDate NullDate) : es

printEvent :: Event -> String
printEvent (Event d es en) = 
	showString (show es)
	$ showString desc
	$ show (show $ en-es-nd)
	where 
		desc = showDesc d
		nd = length desc

printEvents :: [Event] -> String
printEvents [] = ""
printEvents [e] = ""
printEvents (e:lst@(en:es)) = 
	showString (show $ en-e) 
	$ printEvents lst

-- TODO: see about rewriting this function as a type as that's what the goal of
-- this function is.  
-- a task is simply an event that occurs once and doesn't
-- last any time. The EventDate is the task's due date
task :: EventDate -> Event 
task date = Event date date Once

-- a shortcut for creating event dates 
eventDate :: Int -> Int -> Int -> Int -> Int -> EventDate 
eventDate h mi mo d y = EventDate (fromGregorian (toInteger y) mo d) (Time h mi)

-- make an event that will occure Once
-- this checks for cross day times and updates the end date
-- for example if start time is 10/10/10 10:30pm and end time is : 10/10/10
-- 9:00am then the end time will be updated to show 10/11/10 9:00am
{-makeOnceEvent :: EventDate -> EventDate -> Event-}
{-makeOnceEvent s e -}
	{-| (date s) == (date e) && (time s) > (time e) = Event s (addEventDays 1 e) Once-}
	{-| otherwise = Event s e Once-}

{-addEventDays :: Integer -> EventDate -> EventDate-}
{-addEventDays n e = EventDate (addDays n $ date e) (time e)-}

{--- generate all once events for a given event with a certain frequency-}
{--- that occure before an end day-}
{-allEvents :: EventDate -> Event -> [Event]-}
{-allEvents _ e -}
	{-| (eventFreq e) == Once = [e]-}

{-allEvents EventDate {date = ed} (Event {eventFreq = freq,startDate = fsd, endDate = fed})= -}
	{-(\d -> makeOnceEvent (EventDate d (time fsd)) (EventDate d (time fed)))-}
	{-<$> expandDays (onDays freq) (date $ fsd) ed (nDays freq)-}

{--- expandDays takes a list of week days (sunday = 0, Saturday = 7), a start day,-}
{--- an end day and the frequency of the event (every n days) and generates a list-}
{--- of all the days the event happens-}
{-expandDays :: [Int] -> Day -> Day -> Integer -> [Day]-}
{-expandDays _ _ _ 0 = []-}
{-expandDays [] ds de k   = sort $ nub $ genDays k de ds-}
{-expandDays days ds de k = sort $ nub $ concat $ genDays k de . (getFirstDay ds) <$> days-}
	{-where -}
		{-getFirstDay :: Day -> Int -> Day-}
		{-getFirstDay ds d = addDays (toInteger $ (\(_,_,wds) -> firstDay wds d) $ toWeekDate ds) ds-}
		{-firstDay wds d-}
			{-| (wds > d) = 7-wds+d -}
			{-| otherwise = d-wds-}

{--- given the end day, the day of the first occurence, and the frequency of the-}
{--- event generate a list of all of the valid days-}
{-genDays :: Integer -> Day -> Day -> [Day]-}
{-genDays k de d -}
	{-| diffDays de d < 0 = []-}
	{-| otherwise = d : (genDays k de $ addDays k d)-}
