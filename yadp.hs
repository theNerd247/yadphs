{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import System.Environment
import Data.List (sort,partition)
import System.IO.Unsafe
import Data.Time.Calendar

taskData :: String -> [Tasks]
taskData = getLineInfo . T.pack

readDate :: String -> IO EventDate
readDate = return . getEventDate . T.pack 

-- seperate out tasks with and without event info attached
-- tasks with event info are first
seperateEvents :: [Tasks] -> ([Task],[Task])
seperateEvents = Data.List.partition (isEvent . event) . getTasks	
	where	
		isEvent e 
			| e == NoEvent = False
			| otherwise = True

printWeeks :: EventDate -> EventDate -> [Task] -> String
printWeeks ea eb = (formatWeek (a,t1,t2)) . showTasks (edate a) (edate b)
	where 
		a = date ea
		b = date eb
		t1 = time ea
		t2 = time eb
		edate x = EventDate x $ Time 0 0

printDayPlanner :: EventDate -> EventDate -> String -> String
printDayPlanner a b fp = 
	showString (printWeeks a b ets)
	$ showString "\n"
	$ printTTask . sort $ tts
	where 
		ets = fst $ ts fp 
		tts = snd $ ts fp

ts fp = seperateEvents . taskData $ fp

tst = do
	file <- readFile "todo.txt"
	putStrLn $ printDayPlanner sd (addDay 1 ed) file

sd = eventDate 8 0 01 04 2015 
ed = eventDate 23 0 01 24 2015 

main = do
	args <- getArgs
	sd <- readDate (args !! 1)
	ed <- readDate (args !! 2)
	file <- readFile (args !! 0)
	putStrLn $ printDayPlanner sd (addDay 1 ed) file
	return ()
