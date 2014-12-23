{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import Data.Text.Lazy
import Control.Applicative
import Data.Maybe
import System.Environment
import DayDraw
import Data.List (sort)

grabEvents :: String -> [Event]
grabEvents = Prelude.concat . (allEvents <$>) . catMaybes . (evnt <$>) . getEvents . pack

grabEventDate :: String -> IO EventDate
grabEventDate = return . getEventDate . pack 

genRangeEvents :: String -> EventDate -> EventDate -> String
genRangeEvents file sd ed = printAsWeeks $ sortByDays $ filterEvents sd ed $ grabEvents file

main = do
	args <- getArgs
	sd <- grabEventDate (args !! 1)
	ed <- grabEventDate (args !! 2)
	file <- readFile "todo.txt"
	putStrLn $ genRangeEvents file sd ed
	return ()
