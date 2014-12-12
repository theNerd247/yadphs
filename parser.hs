{-# LANGUAGE OverloadedStrings #-}
module Parser
(
csvFile
) where

import Task
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T
import Control.Applicative 
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator
import Data.Digits
import Data.Time.Calendar
import Data.Time.Clock
import System.IO.Unsafe

data ParsedInfo = ParsedInfo Char T.Text (Maybe Event)

-- a testing function 
tst x = maybeResult . (parse x) 

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

parseEvent = Event <$> do
	ds <- parseFrom 

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
	freq <- parseFreq
	option Nothing parseOn

parseOn = do
	string "on"
	skipSpace
	many1 $ choice $ string <$> ["Sun","Mon","Tue","Wed","Thr","Fri","Sat"]

-- TODO: write freq choice code
parseFreq = choice 

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

parseDate :: Parser Date
parseDate = makeDate <$> twoNum "/" 

makeDate :: [Int] -> Date
makeDate [] = Date 0 0 0
makeDate [x] = Date 0 0 0
makeDate [m,d,y] = Date m d y
makeDate [m,d] = Date m d $ cy currentYear m 

cy [y,m] mm 
	| mm < m = y+1
	| otherwise = y

currentYear :: [Int]
currentYear = unsafePerformIO $ (\(y,m,_) -> [lastDigits y, m]) 
	<$> (toGregorian . utctDay) <$> getCurrentTime
	
lastDigits :: Integer -> Int
lastDigits x = unDigits 10 $ drop 2 $ digits 10 $ fromInteger x

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
