{-# LANGUAGE OverloadedStrings #-}
-- TODO: document the parser for heaven's sake!!
module Parser
(
csvFile
) where

import Control.Applicative 
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text.Lazy 
import Data.Char
import Data.Digits
import Data.List
import qualified Data.Text as TT (take)
import qualified Data.Text.Lazy as T
import Data.Time.Calendar
import Data.Time.Clock
import System.IO.Unsafe
import Task

data ParsedInfo = ParsedInfo Char T.Text (Maybe Event)

-- a testing function 
tst x = maybeResult . (parse x) 

-- a parser for things whos order does not matter
noOrder p = many1 $ (skipSpace >> choice p)

instance Show ParsedInfo where
	show (ParsedInfo p d t) = 
		showString (show p) $ 
		showString (" " ++ show d) $
		show t -- our todo.txt file will have many lines

csvFile = many' line

-- parses a task line
line = do 
	p <- option '\0' priority
	d <- description
	t <- option (Nothing) eventInfo 
	option ' ' $ eol
	return (ParsedInfo p d t)

-- our end of line characters
eol = char '\n' 
	<|> char '\r'

-- parses the description of a task
-- TODO: optimize this function
description = do 
	d <- manyTill (anyChar) (eitherP (endOfInput) (string "--")) 
	return $ T.dropWhileEnd (=='\n') $ T.pack d --remove any trailing newline characters

-- parses the priority of our task
priority = do
	char '('
	prio <- satisfy $ inClass "A-Z"	
	char ')'
	return prio

--TODO: write this temp fix
eventInfo = return $ Just . task $ eventDate 0 0 0 0 0

-- TODO: rewrite so that the different parsers can lie out of order in the text
-- stream (a search function maybe needed)
parseEvent = Event <$> do
	frq <- parseFreq
	at <- parseAt
	from <- parseFrom 
	on <- parseOn


parseFrom = do
	string "from"
	skipSpace
	d1 <- parseDate
	pTo
	d2 <- parseDate
	return (d1,d2)

parseAt = do
	string "at"
	skipSpace
	t1 <- parseTime
	pTo
	t2 <- parseTime
	return (t1,t2)

parseEvery = do
	string "every"
	skipSpace
	parseOn

parseOn = do
	string "on"
	skipSpace
	return parseWDay 

parseWDay = mwf
	<|> tr
	<|> weekend
	<|> weekday
	<|> concat <$> noOrder [sun,mon,tue,wed,thu,fri,sat]

-- parsers for which days the event takes place on
mwf = string "MWF" >> return [1,3,5]
tr = string "TR" >> return [2,4]
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
	<|>	(string . TT.take 3) s
	<|> (string . TT.take 2) s

parseFreq = daily
	<|> weekly
	<|> biweekly

-- parsers for the "every n days" of an event
daily = string "daily" >> return 1
weekly = string "weekly" >> return 7
biweekly = string "biweekly" <|> string "every other week" >> return 14

pTo = do
	skipSpace
	string "to" <|> string "-"
	skipSpace

parseTask = task <$> do
	string "due"
	opChar ':'
	skipMany $ char ' '
	parseEventDate

parseEventDate = option (eventDate 0 0 0 0 0) (do 
	d <- parseDate
	skipMany1 $ char ' '
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

