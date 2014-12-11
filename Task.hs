module Task 
(
filterEvents,
eventDate,
task,
Time(Time),
Date(Date),
EventDate(EventDate),
EventFreq,
Event(Event)
) where

import Data.List 
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Text

-- day of week structure
data WDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show,Eq,Ord)

-- time structure
data Time = Time { hour :: Int 
                 , minute :: Int
								 } deriving (Eq,Ord)

instance Show Time where
	show (Time h m) = showString (show h) $ showString ":" $ (show m)

-- date structure
data Date = Date {month :: Int, day :: Int, year :: Int} deriving (Eq,Ord)

instance Show Date where
	show (Date m d y) = 
		showString (show m) $ 
		showString "/" $ 
		showString (show d) $ 
		showString "/" $ 
		(show y)

-- date and time combined into a useable form
data EventDate = EventDate {date :: Date
													 ,time :: Time
													 } deriving (Eq,Ord)
instance Show EventDate where
	show (EventDate d t) = 
		showString (show t) $ 
		showString " " $
		(show d)

-- events can occur at different frequencies (e.g: biweekly, weekly, monthly,
-- yearly etc...) for weekly and biweekly we need to know which days of the week
-- they are on for this to make any sense.
data EventFreq = Once | Daily | Weekly [WDay] | Biweekly [WDay] | Monthly |
	Yearly deriving (Eq,Show,Ord)

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

{-instance Show [Event] where -}

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
eventDate h mi mo d y = EventDate (Date mo d y) (Time h mi)
