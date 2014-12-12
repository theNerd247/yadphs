module Task 
(
filterEvents,
eventDate,
task,
Time(Time),
EventDate(EventDate),
EventFreq,
Event(Event)
) where

import Control.Applicative
import Data.List 
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Text
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

-- time structure
data Time = Time { hour :: Int 
                 , minute :: Int
								 } deriving (Eq,Ord)

instance Show Time where
	show (Time h m) = showString (show h) $ showString ":" $ (show m)

-- date and time combined into a useable form
data EventDate = EventDate {date :: Day
													 ,time :: Time
													 } deriving (Eq,Ord)

instance Show EventDate where
	show (EventDate d t) = 
		showString (show t) $ 
		showString " " $
		(show d)

-- events can occur at different frequencies 
-- an event can occure once, or every n days on a list of days (1 being monday,
-- 7 being sunday) (the second parameter is optional)
data EventFreq = Once | Every {nDays :: Integer, onDays :: [Int]} deriving (Eq,Show,Ord)

-- custom type to handle task info
-- an event is simply the combination of a start and end date, and a frequency
-- at which the even occurs
data Event = Event {startDate :: EventDate
                   , endDate :: EventDate
									 , eventFreq :: EventFreq} deriving (Eq,Ord)

instance Show Event where
	show (Event s e f) = 
		showString "Start: " $ 
		showString (show s) $
		showString " End: " $ 
		showString (show e) $ 
		showString " Feq: " $ 
		(show f)
	showList [] = showString "[No Events]"
	showList (e:[]) = showString (show e)
	showList (e:es) = 
		showString $
		showString (show e) $ 
		showString "\n" $ 
		show es  

-- TODO: see about rewriting this function as a type as that's what the goal of
-- this function is.  
-- a task is simply an event that occurs once and doesn't
-- last any time. The EventDate is the task's due date
task :: EventDate -> Event 
task date = Event date date Once

-- a filter that grabs the events out of an event list such that each event's
-- start date is within the given range
-- the list that is returned is sorted
filterEvents :: EventDate -> EventDate -> [Event] -> [Event]
filterEvents _ _ [] = []
filterEvents start end events = 
	filter (\(Event x _ _) -> (start <= x) && (x <= end)) $ sort events

-- a shortcut for creating event dates 
eventDate :: Int -> Int -> Int -> Int -> Int -> EventDate 
eventDate h mi mo d y = EventDate (fromGregorian (toInteger y) mo d) (Time h mi)

-- make an event that will occure Once
-- this checks for cross day times and updates the end date
-- for example if start time is 10/10/10 10:30pm and end time is : 10/10/10
-- 9:00am then the end time will be updated to show 10/11/10 9:00am
makeOnceEvent :: EventDate -> EventDate -> Event
makeOnceEvent s e 
	| (date s) == (date e) && (time s) > (time e) = Event s (addEventDays 1 e) Once
	| otherwise = Event s e Once

addEventDays :: Integer -> EventDate -> EventDate
addEventDays n e = EventDate (addDays n $ date e) (time e)

allEvents :: Event -> [Event]
allEvents e 
	| (eventFreq e) == Once = [e]
allEvents (Event sd ed freq) = 
	(\d -> makeOnceEvent (EventDate d (time sd)) (EventDate d (time ed)))
	<$> expandDays (onDays freq) (date $ sd) (date $ ed) (nDays freq)


-- expandDays takes a list of week days (sunday = 0, Saturday = 7), a start day,
-- an end day and the frequency of the event (every n days) and generates a list
-- of all the days the event happens
expandDays :: [Int] -> Day -> Day -> Integer -> [Day]
expandDays _ _ _ 0 = []
expandDays [] ds de k   = genDays k de ds
expandDays days ds de k = concat $ genDays k de . (getFirstDay ds) <$> days
	where 
		getFirstDay :: Day -> Int -> Day
		getFirstDay ds d = addDays (toInteger $ (\(_,_,wds) -> firstDay wds d) $ toWeekDate ds) ds
		firstDay wds d
			| (wds > d) = 7-wds+d 
			| otherwise = d-wds

-- given the end day, the day of the first occurence, and the frequency of the
-- event generate a list of all of the valid days
genDays :: Integer -> Day -> Day -> [Day]
genDays k de d 
	| diffDays de d < 0 = []
	| otherwise = d : (genDays k de $ addDays k d)
