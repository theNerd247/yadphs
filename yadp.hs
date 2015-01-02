{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import System.Environment
import Data.List (sort,partition)

taskData :: String -> [Tasks]
taskData = getLineInfo . T.pack

readEventDate :: String -> IO EventDate
readEventDate = return . getEventDate . T.pack 

-- seperate out tasks with and without event info attached
-- tasks with event info are first
seperateEvents :: [Tasks] -> ([Tasks],[Tasks])
seperateEvents = Data.List.partition $ isEvent . event . taskTsk
	where	
		isEvent e 
			| e == NoEvent = False
			| otherwise = True

printData :: EventDate -> EventDate -> [Tasks] -> String
printData a b = formatWeek . showTasks a b . getTasks

tst sd ed file = printData sd ed $ fst $ seperateEvents $ taskData file

main = do
	args <- getArgs
	sd <- readEventDate (args !! 1)
	ed <- readEventDate (args !! 2)
	file <- readFile (args !! 0)
	putStrLn $ printData sd ed $ fst $ seperateEvents $ taskData file
	return ()
