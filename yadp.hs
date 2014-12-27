{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import Data.Text.Lazy
import Control.Applicative
import Data.Maybe
import System.Environment
import DayDraw
import Data.List (sort,partition)

taskData :: String -> [ParsedInfo]
taskData = getLineInfo . pack

readEventDate :: String -> IO EventDate
readEventDate = return . getEventDate . pack 

-- seperate out tasks with and without event info attached
seperateEvents :: [ParsedInfo] -> ([ParsedInfo],[ParsedInfo])
seperateEvents = Data.List.partition $ isJust . evnt

genEvents :: EventDate -> EventDate -> ParsedInfo -> [(String,Event)]
genEvents sd ed p = uniZip (unpack $ desc p) 
	$ (allEvents ed <$>) . filterEvents sd ed . fromJust

uniZip :: a -> [b] -> [(a,b)]
uniZip _ [] = []
uniZip a (x:xs) = (a,x) : uniZip a xs

main = do
	args <- getArgs
	sd <- readEventDate (args !! 1)
	ed <- readEventDate (args !! 2)
	file <- readFile "todo.txt"
	{-putStrLn $ genRangeEvents file sd ed-}
	return ()
