{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import System.Environment
import DayDraw
import Data.List (sort,partition)

taskData :: String -> [ParsedInfo]
taskData = getLineInfo . T.pack

readEventDate :: String -> IO EventDate
readEventDate = return . getEventDate . T.pack 

-- seperate out tasks with and without event info attached
-- tasks with event info are first
seperateEvents :: [ParsedInfo] -> ([ParsedInfo],[ParsedInfo])
seperateEvents = Data.List.partition $ isJust . evnt

genEvents :: EventDate -> EventDate -> [ParsedInfo] -> [[(String,Event)]]
genEvents s e = sortByDays . concat . (genEvent s e <$>)

-- sort a list of events by days
sortByDays :: [(String,Event)] -> [[(String,Event)]]
sortByDays [] = []
sortByDays l@([(s,e)]) = [l]
sortByDays lst@(e:es) = (fst el):(sortByDays $ snd el)
	where 
		el = span sameDate lst
		sameDate = (==) (date $ startDate $ snd e) . date . startDate . snd

genEvent :: EventDate -> EventDate -> ParsedInfo -> [(String,Event)]
genEvent sd ed p = uniZip (T.unpack $ desc p) 
	$ filterEvents sd ed 
	$ allEvents ed
	$ fromJust 
	$ evnt p

printData :: EventDate -> EventDate -> String -> String
printData sd ed s = printAsWeeks 
	$ genEvents sd ed 
	$ fst sepEs
	where sepEs = seperateEvents . taskData $ s

uniZip :: a -> [b] -> [(a,b)]
uniZip _ [] = []
uniZip a (x:xs) = (a,x) : uniZip a xs

main = do
	args <- getArgs
	sd <- readEventDate (args !! 1)
	ed <- readEventDate (args !! 2)
	file <- readFile (args !! 0)
	putStrLn $ printData sd ed file
	return ()
