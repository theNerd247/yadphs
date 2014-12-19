{-# LANGUAGE OverloadedStrings #-}
import Parser 
import Task
import Data.Text.Lazy
import Control.Applicative
import Data.Maybe
import System.Environment

grabEvents :: String -> [Event]
grabEvents = Prelude.concat . (allEvents <$>) . catMaybes . (evnt <$>) . getEvents . pack

grabEventDate :: String -> IO EventDate
grabEventDate = return . getEventDate . pack 

main = do
	args <- getArgs
	sd <- grabEventDate (args !! 1)
	ed <- grabEventDate (args !! 2)
	file <- readFile (args !! 0)
	putStrLn $ show $ filterEvents sd ed $ grabEvents file 
	return ()
