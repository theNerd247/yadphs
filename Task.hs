{-# LANGUAGE OverloadedStrings #-}
module Task 
(
Task(Task,event,prio,desc)
,Tasks(Tasks,taskTsk)
,Priority(Priority,NoPrio)
,task
,Time(Time,hour,minute)
,EventDate(EventDate,date,time)
,EventFreq(Once,Every)
,Event(NoEvent)
,mkEvent
,eventDate
,formatWeek
,showTasks
,getTasks
) where

import Control.Applicative
import Data.List 
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate (toWeekDate)

-- time structure
data Time = Time { hour :: Int 
                 , minute :: Int
								 } deriving (Eq,Ord)

instance Show Time where
	show (Time h m) = showString (showDouble h) $ showString ":" $ (showDouble m)
		where 
			showDouble n
				| n < 10 = '0':(show n)
				| otherwise = show n 

-- date and time combined into a useable form
data EventDate = EventDate 
	{date :: Day
	,time :: Time
	} deriving (Eq,Ord,Show)

-- events can occur at different frequencies 
-- an event can occure once, or every n days on a list of days (1 being monday,
-- 7 being sunday) (the second parameter is optional)
data EventFreq = Once | Every {nDays :: Integer, onDays :: [Int]} deriving (Eq,Show,Ord)

-- custom type to handle task info
-- an event is simply the combination of a start and end date, and a frequency
-- at which the even occurs
-- Note: use toEventNum to create start and end dates for an Event
data Event = NoEvent | Event 
	{eventStart :: Integer
	,eventEnd :: Integer
	,startDate :: EventDate
	,endDate :: EventDate
	} deriving (Eq,Ord,Show)

instance Num Event where
	(-) (Event es en a b) (Event {eventEnd = e}) = Event (es-e) (en-e) a b

toEventNum :: EventDate -> Integer
toEventNum (EventDate d t) =  ld + (toInteger lt)
	where 
		ld = div (60*24*ds) (toInteger minutesPerLine)
		ds = diffDays d (fromGregorian 0 0 0)
		lt = div (60*h+m) minutesPerLine
		h = hour t
		m = minute t

data Priority = NoPrio | Priority {pchar :: Char}

instance Show Priority where
	show NoPrio = "---"
	show (Priority {pchar = c}) = '(':c:")"

data Task = EmptyTask {event :: Event} | Task 
	{prio :: Priority
	,desc :: T.Text
	,event :: Event
	}

instance Show Task where
	show (EmptyTask _) = ""
	show t = printTask t
	showList es = showString "" -- use showTasks instead

instance Eq Task where
	(==) (Task {event = e1}) (Task {event = e2}) = e1 == e2

instance Ord Task where
	(<=) (Task {event = e1}) (Task {event = e2}) = l1 <= l2
		where 
			l1 = startDate e1
			l2 = startDate e2

-- a method to "subtract" tasks
subTask :: Task -> Task -> Task
subTask (Task p d ev) (EmptyTask e) = Task p d (ev-e)
subTask (Task p d ev) (Task _ _ e) = Task p d (ev-e)

printTask :: Task -> String
printTask (Task p d NoEvent) = 
	showString (show p)
	$ showString " "
	$ T.unpack d
printTask (Task p dsc ev) = 
	showString (taskLine p tm es)
	$ showString desc
	$ taskLine NoPrio tme $ en-es-nd
	where 
		desc = formatDesc $ T.unpack dsc
		nd = toInteger . length . lines $ desc
		tm = time . startDate $ ev
		tme = time . endDate $ ev
		es = eventStart ev
		en = eventEnd ev

-- wrapper around printTasks
showTasks a b e = 
	showString t
	$ nblanklines (n)
	where 
		t = (printTasks a b (baseTask:e))
		baseTask = EmptyTask (task a)
		n = linesPerDay-nn
		nn = snd . divMod (length . lines $ t) $ linesPerDay

printTasks :: EventDate -> EventDate -> [Task] -> String
printTasks _ _ [] = ""
printTasks a b [e] = ""
printTasks a b (e:lst@(en:es)) = showString t $ printTasks a b lst
	where 
		t
			| (isValidEvent a b $ event en) == False = ""
			| otherwise = show $ subTask en e

isValidEvent a b (Event {startDate = sd,endDate = ed}) = not $ (ed < a) || (b < sd)

-- take a printed task(s) and format it in the form of a week
-- each col is a day and each row is a week
formatWeek :: String -> String
formatWeek = makeWeek . makeDays . lines

makeDays :: [String] -> [String]
makeDays [] = []
makeDays s = (unlines $ fst d) : makeDays (snd d)
	where 
		d = splitAt linesPerDay s

makeWeek :: [String] -> String
makeWeek [] = ""
makeWeek ds = showString (comDays $ fst dds) 
	$ showString "\n" 
	$ makeWeek (snd dds)
	where	
		comDays = unlines . zipDays . (lines <$>)
		dds = splitAt 7 ds

zipDays :: [[String]] -> [String]
zipDays [d] = d
zipDays (d:ds) = zipWith (++) d (zipDays ds)

vcolChar = '|'
rowChar = '-'
colWidth = 20
minutesPerLine = 15 
linesPerDay = div 1440 minutesPerLine

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
	showString (nblanklines $ n-1)
	$ dayLine
	$ showString (rowChar:"")
	$ showString (show p)
	$ showString (replicate (colWidth-12) rowChar)
	$ showString (show t)
	$ rowChar:""
	where n = fromInteger ns

formatDesc :: String -> String
formatDesc [] = ""
formatDesc d 
	| l < ll = dayLine $ d ++ (replicate (ll-l) ' ')
	| otherwise = (dayLine $ fst spn) ++ (formatDesc $ snd spn)
	where 
		l = length d
		ll = colWidth-2
		spn = splitAt ll d

{-showTasks :: EventDate -> EventDate -> [Task] -> String-}
{-showTasks a b (e:es) -}

-- TODO: see about rewriting this function as a type as that's what the goal of
-- this function is.  
-- a task is simply an event that occurs once and doesn't
-- last any time. The EventDate is the task's due date
task :: EventDate -> Event 
task date = Event n n date date 
	where n = toEventNum date

-- a shortcut for creating event dates 
eventDate :: Int -> Int -> Int -> Int -> Int -> EventDate 
eventDate h mi mo d y = EventDate (fromGregorian (toInteger y) mo d) (Time h mi)

mkEvent :: EventDate -> EventDate -> Event
mkEvent a b = Event (toEventNum a) (toEventNum b) a b

data Tasks = Tasks {evnFreq ::EventFreq, taskTsk ::Task}

-- given a start and end date, and a list of Tasks generate the cooresponding
-- list of tasks
getTasks :: [Tasks] -> [Task]
getTasks [] = []
getTasks ts = sort . concat $ expandTasks <$> ts

expandTasks :: Tasks -> [Task]
expandTasks (Tasks _ (EmptyTask _)) = []
expandTasks (Tasks Once t) = [t]
expandTasks (Tasks freq (Task p d evnt)) = Task p d <$> expandEvents freq evnt

expandEvents :: EventFreq -> Event -> [Event]
expandEvents _ NoEvent = [NoEvent]
expandEvents (Every {nDays = n, onDays = pat}) evnt = (\d -> mkEvent (e1 d) (e2 d)) <$> dys
	where 
		ts = time . startDate $ evnt
		te = time . endDate $ evnt
		ds = date . startDate $ evnt
		de = date . endDate $ evnt
		dys = concat $ genDays (toInteger n) de <$> firstDays ds pat
		e1 d = EventDate d ts
		e2 d = EventDate d te

firstDays :: Day -> [Int] -> [Day]
firstDays d [] = [d]
firstDays d x = firstDay d <$> x

-- given a minimum day and a day of week determine the first valid day
firstDay :: Day -> Int -> Day
firstDay startDay d = addDays (toInteger n) startDay
	where 
		wd = (\(_,_,dd) -> dd) . toWeekDate $ startDay
		n | (wd > d) = 7 
			| otherwise = d-wd

-- create a list of days based on the frequency, the first valid day and the end
-- day
genDays :: Integer -> Day -> Day -> [Day]
genDays k de d 
	| diffDays de d < 0 = []
	| otherwise = d : (genDays k de $ addDays k d)

-- testing data
p = Priority 'A'
dsc = "Blarg and foobar hello world bye bye moon"

a = eventDate 0 0 1 05 2015
b = eventDate 0 0 1 14 2015

evnt1 = mkEvent ea eb
ea = eventDate 10 30 01 05 2015
eb = eventDate 14 45 01 05 2015

evnt2 = mkEvent ea1 eb1
ea1 = eventDate 08 00 01 06 2015
eb1 = eventDate 13 00 02 11 2015

tsk1 = Task p dsc evnt1
tsk2 = Task p dsc evnt2
tsks = [tsk1,tsk2]

evnt3 = mkEvent ea eaa
eaa = eventDate 14 45 01 11 2015

fre = Every 7 [1,3,5,7]
ttsk1 = Tasks fre (Task p dsc evnt3)
ttsk2 = Tasks (Every 2 []) (Task p dsc evnt2)

tt = getTasks [ttsk1,ttsk2]
