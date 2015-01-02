{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import System.Environment
import DayDraw
import Data.List (sort,partition)

taskData :: String -> [Tasks]
taskData = getLineInfo . T.pack

readEventDate :: String -> IO EventDate
readEventDate = return . getEventDate . T.pack 

-- seperate out tasks with and without event info attached
-- tasks with event info are first
seperateEvents :: [Task] -> ([Tasks],[Tasks])
seperateEvents = Data.List.partition $ isNoEvent . event
	where	isNoEvent e 
		| e == NoEvent = True
		| otherwise = False

genEvents :: EventDate -> EventDate -> [Tasks] -> [Task]
genEvents s e = sort . concat . (genEvent s e <$>)

tst :: EventDate -> EventDate -> String -> [[Yadp]]
tst sd ed file = genEvents sd ed 
	$ fst
	$ seperateEvents
	$ taskData file

main = do
	args <- getArgs
	sd <- readEventDate (args !! 1)
	ed <- readEventDate (args !! 2)
	file <- readFile (args !! 0)
	putStrLn $ printData sd ed file
	return ()
