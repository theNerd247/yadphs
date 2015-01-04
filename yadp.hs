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

readDate :: String -> IO Day
readDate = return . getDate . T.pack 

-- seperate out tasks with and without event info attached
-- tasks with event info are first
seperateEvents :: [Tasks] -> ([Tasks],[Tasks])
seperateEvents = Data.List.partition $ isEvent . event . taskTsk
	where	
		isEvent e 
			| e == NoEvent = False
			| otherwise = True

printData :: Day -> Day -> [Tasks] -> String
printData a b = formatWeek . showTasks (edate a) (edate b) . getTasks

edate x = EventDate x $ Time 0 0 

printWeeks sd ed file = printData sd ed $ fst $ seperateEvents $ taskData file

f = unsafePerformIO $ readFile "todo.txt"
sd = fromGregorian 2015 01 05
ed = fromGregorian 2015 01 19

prTst = putStrLn $ printWeeks sd ed f

main = do
	args <- getArgs
	sd <- readDate (args !! 1)
	ed <- readDate (args !! 2)
	file <- readFile (args !! 0)
	putStrLn $ printWeeks sd ed file
	return ()
