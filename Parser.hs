{-# LANGUAGE OverloadedStrings #-}
-- TODO: document the parser for heaven's sake!!
module Parser
(
getLineInfo,
getEventDate,
ParsedInfo(ParsedInfo,prio,desc,evnt)
) where

import Control.Applicative 
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char
import Data.Digits
import Data.List
import qualified Data.Text as T 
import Data.Time.Calendar
import Data.Time.Clock
import System.IO.Unsafe
import Task

data ParsedInfo = ParsedInfo {prio :: Char, desc :: T.Text, evnt :: Maybe Event}

-- a testing function 
tst x = maybeResult . (parse x) 

-- a parser for multiple things whos order does not matter
-- those things must be of the same time
noOrder p = many1 $ (skipSpace >> choice p)

-- optional shortcut using Maybe monad
possibly p = option Nothing (Just p)

instance Show ParsedInfo where
	show (ParsedInfo p d t) = 
		showString (show p) $ 
		showString (" Desc: " ++ show d) $
		show t -- our todo.txt file will have many lines

getLineInfo = evs [] . parse parseFile 
getEventDate = evs (eventDate 0 0 0 0 0) . parse parseEventDate 

evs a (Partial e) = evs a $ e ""
evs a (Fail _ _ _) = a
evs a (Done i r) = r

-- parses a given file's text
-- and then applies a given function to process the results
parseFile = manyTill line endOfInput

t1 = do 
	l1 <- line
	l2 <- line
	return (l1,l2)

-- parses a task line
line = do 
	option ' ' eol
	p <- option '\0' priority
	d <- description
	t <- option (Nothing) eventInfo 
	return (ParsedInfo p d t)

-- our end of line characters
eol = char '\n' 
	<|> char '\r'

-- parses the description of a task
-- TODO: optimize this function
description = takeTill e 
	where e c = c == '\n' || c == '-'
	{-skipSpace-}
	{-return $ T.dropWhileEnd (=='\n') $ T.pack d --remove any trailing newline characters-}

-- parses the priority of our task
priority = do
	char '('
	prio <- satisfy $ inClass "A-Z"	
	char ')'
	return prio

eventInfo = string "-" *> skipSpace *> (Just <$> parseEvent)

-- TODO: rewrite so that the different parsers can lie out of order in the text
-- stream (a search function maybe needed)
parseEvent = parseTask <|> do
	frq <- parseFreq -- daily,weekly,etc.
	skipSpace
	at <- parseAt -- at Time - Time
	skipSpace
	from <- parseFrom  -- from Date - Date
	return $ Event (EventDate (fst from) (fst at)) (EventDate (snd from) (snd at)) frq

parseFrom = do
	string "from" <|> string "on"
	skipSpace
	d1 <- parseDate 
	d2 <- option d1 (do
		pTo
		d <- parseDate
		return d)
	return (d1,d2)

parseAt = option (Time 0 0,Time 0 0) $ do
	string "at"
	skipSpace
	t1 <- parseTime
	skipSpace
	pTo
	skipSpace
	t2 <- parseTime
	return (t1,t2)

parseFreq = option Once $ do
	n <- parseNDays 
	skipSpace
	d <- parseOnDays
	return $ Every n d

parseNDays = daily
	<|> weekly
	<|> biweekly
	<|> nweeks

parseOnDays = option [] $ do
	string "on"
	skipSpace
	parseWDay 

parseWDay =	choice [mwf,tr,weekend,weekday]
	<|> concat <$> noOrder [sun,mon,tue,wed,thu,fri,sat]

-- parsers for which days the event takes place on
mwf = string "MWF" >> return [1,3,5]
tr = string "TR" >> return [2,4]
everyday = string "everyday" >> return [1..7]
weekend = string "weekends" >> return [6,7]
weekday = string "weekdays" >> return [1..5]
sun = day "sunday" <|> day "Sunday" >> return [7]
mon = day "monday" <|> day "Monday" >> return [1]
tue = day "tueday" <|> day "Tueday" >> return [2]
wed = day "wednesday" <|> day "Wednesday" >> return [3]
thu = day "thursday" <|> day "Thursday" >> return [4]
fri = day "friday" <|> day "Friday" >> return [5]
sat = day "saturday" <|> day "Saturday" >> return [6]

day s = string s 
	<|>	(string . T.take 3) s
	<|> (string . T.take 2) s

-- parsers for the "every n days" of an event
daily = string "daily" >> return 1
weekly = string "weekly" <|> string "every week" >> return 7
biweekly = string "biweekly" <|> string "every other week" >> return 14
nweeks = do 
	string "every"
	skipSpace
	d <- toInteger . digitToInt <$> digit
	skipSpace
	string "week" <|> string "weeks"
	return (d*7)

pTo = do
	skipSpace
	string "to" <|> string "-"
	skipSpace

parseTask = task <$> do
	string "due"
	opChar ':'
	skipSpace
	parseEventDate

parseEventDate = option (eventDate 0 0 0 0 0) (do 
	d <- parseDate
	skipSpace
	t <- option (Time 0 0) parseTime
	return $ EventDate d t)

parseDate :: Parser Day
parseDate = makeDate <$> twoNum "/" 

makeDate :: [Int] -> Day
makeDate [] = fromGregorian 0 0 0
makeDate [x] = fromGregorian 0 0 0
makeDate [m,d,y] = fromGregorian  (checkYear y) m d
makeDate [m,d] = fromGregorian (toInteger $ cy currentYear m) m d

-- sometimes the year is written as 10 instead of 2010....account for this.
-- 2000 is hard coded in because chances are this program will be obsolete in
-- year 3000 (and events in 1900's make no sense)
checkYear :: Int -> Integer
checkYear y
	| y < 2000 = toInteger y+2000
	| otherwise = toInteger y

-- increase the year if the month given is earlier than the current month
-- this is for dates that refernce the next year like 1/1 as New years and the
-- current month is june. it makes no sense to have a task/event due that's in
-- the past. 
cy [y,m] mm 
	| mm < m = y+1
	| otherwise = y

currentYear :: [Int]
currentYear = unsafePerformIO $ (\(y,m,_) -> [fromInteger y, m]) 
	<$> (toGregorian . utctDay) <$> getCurrentTime
	
-- parses text in hh:mm format
parseTime :: Parser Time
parseTime = makeTime <$> do 
	nums <- twoNum ":"
	n <- string "am" <|> string "pm"
	return $ miliTime nums n 

-- corrects the given time [hh,mm] for a given am/pm value to military time
miliTime [] _ = []
miliTime t "am" = t
miliTime (12:nums) "pm" = (12:nums)
miliTime (h:nums) "pm" = (h+12:nums)

makeTime :: [Int] -> Time
makeTime [] = Time 0 0
makeTime [h] = Time h 0
makeTime [h,m] = Time h m

-- grabs returns a list of numbers in the form: 
-- ab<sep>cd...<sep>ef -> [ab,cd,ef]
twoNum s = sepBy1 (unDigits 10 <$> (many1 $ digitToInt <$> digit)) (string s)

opChar = option ' ' . char

