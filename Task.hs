{-# LANGUAGE OverloadedStrings #-}
module Task 
(
Task(Task,event,prio,desc)
,Tasks(Tasks,taskTsk)
,Priority(Priority,NoPrio)
,task
,Time(Time,hour,minute)
,EventDate(EventDate,date,time)
,Day
,EventFreq(Once,Every)
,Event(NoEvent)
,mkEvent
,eventDate
,formatWeek
,showTasks
,getTasks
,printTTask
,addDay
,isValidEvent
) where

import Control.Applicative
import Data.List 
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format

-- time structure
data Time = Time { hour :: Int 
                 , minute :: Int
								 } deriving (Eq,Ord)

instance Show Time where
	show (Time h m) = showString (showDouble $ normHour h) $ showString ":" $ (showDouble m)
		where 
			showDouble n
				| n < 10 = '0':(show n)
				| otherwise = show n 
			normHour h
				| h > 12 = h-12
				| otherwise = h

-- date and time combined into a useable form
data EventDate = EventDate 
	{date :: Day
	,time :: Time
	} deriving (Eq,Ord,Show)

addDay :: Integer -> EventDate -> EventDate
addDay n (EventDate d t) = EventDate (addDays n d) t

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
	} deriving (Eq,Show)

instance Num Event where
	(-) (Event es en a b) (Event {eventEnd = e}) = Event (es-e) (en-e) a b

instance Ord Event where
	(<=) NoEvent _ = True
	(<=) _ NoEvent = False
	(<=) e1 e2 = s1 <= s2
		where 
			s1 = startDate e1
			s2 = startDate e2

toEventNum :: EventDate -> Integer
toEventNum (EventDate d t) =  ld + lt + ltm
	where 
		ld = dv (60*24*ds) (toInteger minutesPerLine)
		ds = diffDays d (fromGregorian 0 0 0)
		lt = dv (60*h) minutesPerLine
		ltm = dv m minutesPerLine
		h = hour t
		m = minute t

dv a = round . ((realToFrac a)/) . realToFrac

data Priority = NoPrio | Priority {pchar :: Char} deriving (Eq)

instance Show Priority where
	show NoPrio = "---"
	show (Priority {pchar = c}) = '(':c:")"

instance Ord Priority where
	(<=) NoPrio _ = False
	(<=) _ NoPrio = True
	(<=) (Priority a) (Priority b) = a <= b

data Task = EmptyTask {event :: Event} | Task 
	{prio :: Priority
	,desc :: T.Text
	,event :: Event
	}

instance Show Task where
	show (EmptyTask e) = nblanklines . fromInteger . eventStart $ e
	show t = printTask t  -- ony useful for Tasks with NoEvent
	showList [] = showString ""
	showList (e:es) = showString
		$ showString (show $ prio e) 
		$ show es

instance Eq Task where
	(==) (Task {prio =p1,event = e1}) (Task {prio = p2,event = e2}) = e1 == e2 && p1 == p2

instance Ord Task where
	(<=) (Task {prio = p1,event = e1}) (Task {prio = p2, event = e2})
		| e1 == e2 = p1 <= p2
		| otherwise = e1 <= e2

-- a method to "subtract" tasks
subTask :: Task -> Task -> Task
subTask (EmptyTask e) (Task p d ev) = EmptyTask (e-ev)
subTask (Task p d ev) (EmptyTask e) = Task p d (ev-e)
subTask (Task p d ev) (Task _ _ e) = Task p d (ev-e)

printTTask :: [Task] -> String
printTTask [] = ""
printTTask (t:ts) = showString (show t) $ showString "\n" $ printTTask ts

printTask :: Task -> String
printTask (Task p d NoEvent) = 
	showString (show p)
	$ showString " "
	$ delete ' ' $ T.unpack d
printTask t@(Task p dsc ev) 
	| (startDate ev == endDate ev) = showTask t
	| otherwise = showEvent t

-- TODO: replace taskline with printing start and end time on same line
-- and make the end line just a regular line
showTask (Task p dsc ev) = 
	showString (nblanklines $ n-1)
	$ dayLine
	$ showString (rowChar:"")
	$ showString (show p)
	$ showString (rowChar:"")
	$ showString (take (colWidth-14) $ T.unpack dsc)
	$ showString (rowChar:"")
	$ showString (show t)
	$ rowChar:""
	where	
 		n = fromInteger . eventStart $ ev
		t = time . startDate $ ev

showEvent (Task p dsc ev) =
	showString (taskLine p tm tme $ es)
	$ showString desc
	$ lineAt $ en-es-nd-1
	where 
		desc = formatDesc (fromInteger $ en-es-1) $ T.unpack dsc
		nd = toInteger . length . lines $ desc
		tm = time . startDate $ ev
		tme = time . endDate $ ev
		es = eventStart ev
		en = eventEnd ev

-- wrapper around printTasks
showTasks :: EventDate -> EventDate -> [Task] -> String
showTasks _ _ [] = ""
showTasks a b es = printTasks a b (baseTask:e++[endTask])
	where 
		e = filter (isValidEvent a b . event) es
		baseTask = EmptyTask (task a)
		endTask = EmptyTask (task b)

printTasks :: EventDate -> EventDate -> [Task] -> String
printTasks _ _ [] = ""
printTasks a b [e] = ""
printTasks a b (e:lst@(en:es)) = 
	showString t 
	$ printTasks a b lst
	where 
		t
			| es == [] = s 
			| hlss == 0 = unlines . init . lines $ s
			| otherwise = s
		s = show $ subTask en e
		hlss = eventStart . event $ subTask hls en
		hls = head es

-- tests whether the given event intersects with the given time range where a <
-- b
isValidEvent :: EventDate -> EventDate -> Event -> Bool
isValidEvent a b (Event {startDate = sd,endDate = ed}) = not $ (ed < a) || (b < sd)

printDate :: Day -> String
printDate d = 
	showString " "
	$ showString wd
	$ showString (replicate n ' ')
	$ showString tm
	$ " \n"
	where 
		locale = defaultTimeLocale
		wd = formatTime locale "%A" d
		tm = formatTime locale "%D" d
		n = colWidth - 2 - ((length wd)+(length tm))

-- take a printed task(s) and format it in the form of a week
-- each col is a day and each row is a week
formatWeek :: (Day,Time,Time) -> String -> String
formatWeek dd@(d,t1,t2) = (makeWeek t1 t2) . (makeDays d) . lines

makeDays :: Day -> [String] -> [String]
makeDays _ [] = []
makeDays dd s = (printDate dd ++ (unlines $ fst d)) : makeDays (addDays 1 dd) (snd d)
	where 
		d = splitAt linesPerDay s

onlyTimes :: Time -> Time -> String -> String
onlyTimes t1 t2 s = hd ++ "\n" ++ (unlines . take (l2-l1) . drop (l1+1) $ ls)
	where
		hd = head ls
		ls = lines s
		l1 = fromInteger $ toEventNum (EventDate (fromGregorian 0 0 0) t1)
		l2 = fromInteger $ toEventNum (EventDate (fromGregorian 0 0 0) end)
		end 
			| t2 == (Time 0 0) = Time 24 00
	 		| otherwise = t2

makeWeek :: Time -> Time -> [String] -> String
makeWeek _ _ [] = ""
makeWeek t1 t2 ds = showString (onlyTimes t1 t2 da)
	$ showString "\n" 
	$ makeWeek t1 t2 (snd dds)
	where	
		da = comDays $ dayTimes:(fst dds)
		comDays = unlines . zipDays . (lines <$>)
		dds = splitAt 7 ds

zipDays :: [[String]] -> [String]
zipDays [d] = d
zipDays (d:ds) = zipWith (++) d (zipDays ds)

dayTimes :: String
dayTimes = "      \n" ++ t 1
	where 
		t n 
			| n == linesPerDay = tm $ linesPerDay*minutesPerLine 
			| otherwise = showString (tm $ n*minutesPerLine) $ t (n+1)
		tm x = showString (show $ toTime x) " \n"

toTime n = Time h m
	where 
		h = div n 60
		m = n-(h*60)

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

taskLine :: Priority -> Time -> Time -> Integer -> String
taskLine p t te ns = 
	showString (nblanklines $ n-1)
	$ dayLine
	$ showString (rowChar:"")
	$ showString (show p)
	$ showString (replicate (colWidth-18) rowChar)
	$ showString (show t)
	$ showString (rowChar:"")
	$ showString (show te)
	$ rowChar:""
	where n = fromInteger ns

formatDesc :: Int -> String -> String
formatDesc _ [] = ""
formatDesc n d 
	| l < ll = dayLine $ d ++ (replicate (ll-l) ' ')  -- add space if end of line
	| otherwise = (dayLine $ fst spn) ++ (formatDesc (n-1) $ snd spn)
	where 
		l = length d
		ll = colWidth-2
		spn = splitAt ll $ take (n*ll) d

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

data Tasks = Tasks {evnFreq ::EventFreq, taskTsk ::Task} deriving (Show)

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
b = eventDate 0 0 1 06 2015

evnt1 = mkEvent ea eb
ea = eventDate 11 00 01 05 2015
eb = eventDate 11 50 01 05 2015

evnt2 = mkEvent ea1 eb1
ea1 = eventDate 12 00 01 05 2015
eb1 = eventDate 13 50 01 05 2015

tsk1 = Task p dsc evnt1
tsk2 = Task p dsc evnt2
tsks = [tsk1,tsk2]

evnt3 = mkEvent ea eaa
eaa = eventDate 14 45 01 11 2015

fre = Every 7 [1,3,5,7]
ttsk1 = Tasks fre (Task p dsc evnt1)
ttsk2 = Tasks (Every 2 []) (Task p dsc evnt2)
ttsk3 = Tasks Once (Task p dsc NoEvent)

tt = getTasks [ttsk1,ttsk2,ttsk3]

{-ptst = putStrLn $ formatWeek $ showTasks a b tt-}
