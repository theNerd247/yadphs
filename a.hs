{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T
import Control.Applicative 
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator

data ParsedInfo = ParsedInfo Char T.Text (Date,Time)

instance Show ParsedInfo where
	show (ParsedInfo p d (dt,t)) = 
		showString (show p) $ 
		showString (" " ++ show d) $
		showString (show dt) $
		show t -- our todo.txt file will have many lines
csvFile = many' line

-- parses a task line
line = do 
	p <- option '\0' priority
	d <- description
	t <- option ([],[]) eventInfo 
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
	prio <- upper
	char ')'
	return prio

-- an uppercase character
upper = satisfy $ inClass "A-Z"

-- parses event stuff 
eventInfo = do
	d <- skipMany (char ' ') >> twoNum "/"
	t <- option [0] $ skipMany1 (char ' ') >> twoNum ":"
	return ((d,t))
	
-- grabs returns a list of numbers in the form: 
-- ab<sep>cd...<sep>ef -> [ab,cd,ef]
twoNum s = sepBy1 
	(do
		x <- count 2 $ digitToInt <$> digit
		return (oneDig x))
	(string s)

oneDig :: (Num a) => [a] -> a
oneDig [] = 0
oneDig [x] = x
oneDig (x:y:xs) = (x*10)+y
